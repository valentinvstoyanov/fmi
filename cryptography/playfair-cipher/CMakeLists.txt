cmake_minimum_required(VERSION 3.15)
project(playfair_cipher)

set(CMAKE_CXX_STANDARD 14)

add_executable(playfair_cipher main.cpp random_coder.h)