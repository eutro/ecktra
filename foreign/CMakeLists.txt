project(Ecktra)

cmake_minimum_required(VERSION 3.19)

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBAV REQUIRED IMPORTED_TARGET
  libswresample
  libavcodec
  libavutil
  libavformat
)

add_library(ecktra SHARED ecktra.cc)
target_link_options(ecktra PRIVATE -static-libgcc -static-libstdc++)
target_link_libraries(ecktra PkgConfig::LIBAV)
install(
  TARGETS ecktra
  LIBRARY DESTINATION ${CMAKE_SOURCE_DIR}/../lib)
