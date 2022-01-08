#ifndef ECKTRA_AUDIO_STREAM_H
#define ECKTRA_AUDIO_STREAM_H

#include <algorithm>
#include <atomic>
#include <condition_variable>
#include <vector>
#include <thread>
#include <mutex>
#include <deque>
#include <future>

#include "Av.h"

namespace ecktra {
  struct AudioStream {
    WithCleanup<AVFormatContext> format;
    WithCleanup<AVCodecContext> codec;
    WithCleanup<SwrContext> swr;
    WithCleanup<AVPacket> packet;
    WithCleanup<AVFrame> frame;
    std::vector<uint8_t> buf;

    static int decode(
      int sample_rate,
      const char *url,
      std::shared_ptr<AudioStream> **out
    ) {
      av_register_all();
      int err;

      AVFormatContext *fmt = nullptr;
      err = avformat_open_input(&fmt, url, nullptr, nullptr);
      if (err < 0) return err;

      WithCleanup<AVFormatContext> format(fmt, avformat_close_input);
      err = avformat_find_stream_info(format.get(), nullptr);
      if (err < 0) return err;

      AVCodec *decoder = NULL;
      int streamNo = err = av_find_best_stream(format.get(), AVMEDIA_TYPE_AUDIO, -1, -1, &decoder, 0);
      if (err < 0) return err;

      AVStream *stream = format->streams[streamNo];
      WithCleanup<AVCodecContext> codec(avcodec_alloc_context3(nullptr), avcodec_free_context);
      avcodec_parameters_to_context(codec.get(), stream->codecpar);
      err = avcodec_open2(codec.get(), decoder, nullptr);
      if (err < 0) return err;

      WithCleanup<SwrContext> swr(swr_alloc(), swr_free);
      av_opt_set_int(swr.get(), "in_channel_count",  codec->channels, 0);
      av_opt_set_int(swr.get(), "out_channel_count", 1, 0);
      av_opt_set_int(swr.get(), "in_channel_layout",  codec->channel_layout, 0);
      av_opt_set_int(swr.get(), "out_channel_layout", AV_CH_LAYOUT_MONO, 0);
      av_opt_set_int(swr.get(), "in_sample_rate", codec->sample_rate, 0);
      err = av_opt_set_int(swr.get(), "out_sample_rate", sample_rate, 0);
      if (err < 0) return err;
      av_opt_set_sample_fmt(swr.get(), "in_sample_fmt",  codec->sample_fmt, 0);
      av_opt_set_sample_fmt(swr.get(), "out_sample_fmt", AV_SAMPLE_FMT_DBL,  0);

      err = swr_init(swr.get());
      if (err < 0) return err;

      WithCleanup<AVPacket> packet(av_packet_alloc(), av_packet_free);
      WithCleanup<AVFrame> frame(av_frame_alloc(), av_frame_free);

      *out = new std::shared_ptr<AudioStream>(new AudioStream{
          std::move(format),
          std::move(codec),
          std::move(swr),
          std::move(packet),
          std::move(frame),
          {}
        });
      return 0;
    }

    int read(double **ret_buf, int *ret_bufsz) {
      int err;
      while ((err = av_read_frame(format.get(), packet.get())) >= 0) {
        err = avcodec_send_packet(codec.get(), packet.get());
        if (err == AVERROR(EAGAIN)) continue; // read another frame
        if (err < 0) return err;

        err = avcodec_receive_frame(codec.get(), frame.get());
        if (err == AVERROR(EAGAIN)) continue; // write more input
        if (err < 0) return err;

        break;
      }
      if (err < 0) return err;

      int bufsz = err = av_samples_get_buffer_size(
        nullptr,
        1,
        frame->nb_samples,
        AV_SAMPLE_FMT_DBL,
        0
      );
      if (err < 0) return err;

      buf.resize(bufsz);
      uint8_t *bufs[1] = {&buf.front()};
      int frame_count = err = swr_convert(
        swr.get(),
        bufs,
        frame->nb_samples,
        (const uint8_t**) frame->data,
        frame->nb_samples
      );
      if (err < 0) return err;

      *ret_buf = (double *) &buf.front();
      *ret_bufsz = frame->nb_samples;
      return 0;
    }
  };

  struct BufferedAudioStream {
    std::shared_ptr<AudioStream> as;
    std::mutex lock;
    std::condition_variable reads;
    std::deque<double> buf;
    bool closing = 0;
    std::thread t;
    int err = 0;
    BufferedAudioStream(std::shared_ptr<AudioStream> *ptr):
      as(*ptr),
      t(&BufferedAudioStream::run, this)
    {}
    ~BufferedAudioStream() {
      closing = true;
      t.join();
    }
    void run() {
      while (!closing) {
        double *asbuf;
        int assz;
        int ret = as->read(&asbuf, &assz);

        {
          const std::lock_guard<std::mutex> _(lock);
          if (ret < 0) {
            err = ret;
          } else {
            std::copy(asbuf, asbuf + assz, std::back_inserter(buf));
          }
        }
        reads.notify_all();
        if (err) break;
      }
    }
    int current_buffered() {
      std::lock_guard<std::mutex> _(lock);
      return buf.size();
    }
    int ensured_buffered(int *n) {
      std::unique_lock<std::mutex> ul(lock);
      int e = 0;
      reads.wait(ul, [&e, n, this](){
        if (buf.size() >= *n) {
          return true;
        }
        if (err) {
          e = err;
          return true;
        }
        return false;
      });
      if (e) {
        *n = *n - buf.size();
      }
      return e;
    }
    int buf_read(double *out, int *count) {
      std::lock_guard<std::mutex> _(lock);
      int copied = std::min(buf.size(), (size_t) *count);
      std::copy(buf.begin(), buf.begin() + copied, out);
      buf.erase(buf.begin(), buf.begin() + copied);
      *count -= copied;
      return err;
    }
  };
}

#endif
