cmake_minimum_required(VERSION 3.15)
project(puzzle)

set(CMAKE_CXX_STANDARD 14)

add_executable(puzzle main.cpp board.h board_builder.h puzzle_solver.h)