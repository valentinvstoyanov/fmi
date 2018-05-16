cmake_minimum_required(VERSION 3.10)
project(fmibook)

set(CMAKE_CXX_STANDARD 11)

add_executable(fmibook main.cpp fmibook.cpp fmibook.h data/model/user.cpp data/model/user.h ds/user_array.cpp ds/user_array.h data/model/post.cpp data/model/post.h data/model/text_post.cpp data/model/text_post.h data/model/picture_post.cpp data/model/picture_post.h data/model/link_post.cpp data/model/link_post.h data/repository/user_repository.cpp data/repository/user_repository.h)