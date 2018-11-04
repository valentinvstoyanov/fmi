cmake_minimum_required(VERSION 3.12)
project(conga_line)

set(CMAKE_CXX_STANDARD 14)

add_executable(conga_line student.cc student.h conga_line.cc conga_line.h conga_collection.cc conga_collection.h main.cc)