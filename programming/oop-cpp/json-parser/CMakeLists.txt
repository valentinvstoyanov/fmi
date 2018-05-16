cmake_minimum_required(VERSION 3.10)
project(json_parser)

set(CMAKE_CXX_STANDARD 11)

add_executable(json_parser main.cpp ds/mystring.cpp ds/mystring.h ds/pair.cpp ds/pair.h models/json_value.cpp models/json_value.h models/json_null.cpp models/json_null.h models/json_string.cpp models/json_string.h models/json_boolean.cpp models/json_boolean.h models/json_number.cpp models/json_number.h models/json_array.cpp models/json_array.h parser/json_parser.cpp parser/json_parser.h models/json_object.cpp models/json_object.h)