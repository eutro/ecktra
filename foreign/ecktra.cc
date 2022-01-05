#include <bit>
#include <cerrno>
#include <cstring>
#include <cstdint>
#include <memory>
#include <vector>
#include <bitset>
#include <limits>

#include <complex>
#include "fft.cc"

extern "C" {
#include <libavutil/samplefmt.h>
#include <libavformat/avio.h>
#include <libavutil/error.h>
#include <libavutil/mem.h>
#include <libavutil/opt.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswresample/swresample.h>
}

#define BUF_SIZE (1<<12)

template <typename T>
struct IndirectFreeFn {
  void (*free)(T**);
  IndirectFreeFn(void (*free)(T**)): free(free) {}
  void operator()(T *ptr) {
    if (ptr) free(&ptr);
  }
};
template <typename T>
using WithCleanup = std::unique_ptr<T, IndirectFreeFn<T>>;

struct AudioStream {
  WithCleanup<AVFormatContext> format;
  WithCleanup<AVCodecContext> codec;
  WithCleanup<SwrContext> swr;
  WithCleanup<AVPacket> packet;
  WithCleanup<AVFrame> frame;
  std::vector<uint8_t> buf;
  int read(double **buf, int *bufsz);
};

extern "C" {

char *ecktra_err2str(int err) {
  static char errbuf[AV_ERROR_MAX_STRING_SIZE] = {0};
  errno = 0;
  if (av_strerror(err, errbuf, sizeof(errbuf)) < 0) {
    if (errno) return strerror(errno);
  }
  return errbuf;
}

void ecktra_stream_close(AudioStream *stream) {
  delete stream;
}

int ecktra_eof = AVERROR_EOF;

int ecktra_stream_read(AudioStream *stream, double **buf, int *bufsz) {
  return stream->read(buf, bufsz);
}

int ecktra_decode_audio_file(int sample_rate, const char *url, AudioStream **out) {
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

  *out = new AudioStream{
    std::move(format),
    std::move(codec),
    std::move(swr),
    std::move(packet),
    std::move(frame),
    {}
  };
  return 0;
}

int ecktra_fft_mag(double *vec, int len, int inverse) {
  std::unique_ptr<std::complex<double>[]> ret(new std::complex<double>[len]);
  std::copy(vec, vec + len, ret.get());
  // popcount != 1 => not a power of 2
  std::bitset<std::numeric_limits<unsigned int>::digits> set(len);
  if (set.count() != 1) {
    return 1;
  }
  fft::FFT(inverse ? -1 : 1, len, log2(len), ret.get());
  for (auto *ptr = ret.get(); ptr != ret.get() + len; ++ptr) {
    *vec++ = std::abs(*ptr);
  }
  return 0;
}

}

int AudioStream::read(double **ret_buf, int *ret_bufsz) {
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
