cmake_minimum_required(VERSION 3.10)
project(polynomial)

set(CMAKE_CXX_STANDARD 11)

add_executable(polynomial main.cpp ds/array.cc ds/array.h polynomial.h)