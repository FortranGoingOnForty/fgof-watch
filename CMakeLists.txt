cmake_minimum_required(VERSION 3.25)

project(fgof_watch VERSION 0.1.0 LANGUAGES Fortran C)

add_library(fgof_watch
  src/fgof_watch_types.f90
  src/fgof_watch.f90
  src/fgof_watch_poll.c
)
add_library(fgof::watch ALIAS fgof_watch)

set_target_properties(fgof_watch PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mod"
)

target_include_directories(fgof_watch PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
)

target_compile_options(fgof_watch PRIVATE
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2018 -Wall -Wextra -Wpedantic -Werror>
  $<$<COMPILE_LANG_AND_ID:C,GNU,Clang,AppleClang>:-Wall -Wextra -Wpedantic -Werror>
)
