#include <algorithm>
#include <bitset>

#include "AudioStream.h"

#include <cmath>
#include "Fft.cc"

using namespace ecktra;

extern "C" {
  int ecktra_eof = AVERROR_EOF;

  char *ecktra_err2str(int err) {
    static char errbuf[AV_ERROR_MAX_STRING_SIZE] = {0};
    errno = 0;
    if (av_strerror(err, errbuf, sizeof(errbuf)) < 0) {
      if (errno) return strerror(errno);
    }
    return errbuf;
  }

  int ecktra_decode_audio_file(
    int sample_rate,
    const char *url,
    std::shared_ptr<AudioStream> **out
  ) {
    return AudioStream::decode(sample_rate, url, out);
  }

  int ecktra_stream_read(
    std::shared_ptr<AudioStream> *stream,
    double **buf,
    int *bufsz
  ) {
    return (*stream)->read(buf, bufsz);
  }

  void ecktra_stream_close(
    std::shared_ptr<AudioStream> *stream
  ) {
    delete stream;
  }

  BufferedAudioStream *ecktra_start_buffering(
    std::shared_ptr<AudioStream> *stream
  ) {
    return new BufferedAudioStream(stream);
  }

  int ecktra_buffered_stream_read(BufferedAudioStream *stream, double *buf, int *cnt, int destroy) {
    return stream->buf_read(buf, cnt, destroy);
  }

  int ecktra_buffered_stream_current_buffered(BufferedAudioStream *stream) {
    return stream->current_buffered();
  }

  int ecktra_buffered_stream_ensure_buffered(BufferedAudioStream *stream, int *n) {
    return stream->ensured_buffered(n);
  }

  BufferedAudioStream *ecktra_buffered_stream_fork(BufferedAudioStream *stream) {
    return stream->fork();
  }

  void ecktra_buffered_stream_close(BufferedAudioStream *stream) {
    delete stream;
  }

  int ecktra_fft(double *real, double *imag,
                 int len, int inverse,
                 double *mag, double *arg) {
    // popcount != 1 => not a power of 2
    std::bitset<std::numeric_limits<unsigned int>::digits> set(len);
    if (set.count() != 1) {
      return 1;
    }
    std::unique_ptr<double[]> realAlloced, imagAlloced;
    if (!real) {
      real = (realAlloced = std::unique_ptr<double[]>(new double[len])).get();
      std::fill(real, real + len, 0);
    }
    if (!imag) {
      imag = (imagAlloced = std::unique_ptr<double[]>(new double[len])).get();
      std::fill(imag, imag + len, 0);
    }
    fft::FFT(inverse ? -1 : 1, len, log2(len), real, imag);
    if (arg || mag) {
      for (int i = 0; i < len; ++i) {
        double re = real[i];
        double im = imag[i];
        if (arg) arg[i] = std::atan2(re, im);
        if (mag) mag[i] = std::hypot(re, im);
      }
    }
    return 0;
  }
}
