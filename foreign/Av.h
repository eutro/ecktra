#ifndef ECKTRA_AV_H
#define ECKTRA_AV_H

#include <memory>

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

#endif
