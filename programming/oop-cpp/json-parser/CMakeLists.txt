cmake_minimum_required(VERSION 3.10)
project(json_parser)

set(CMAKE_CXX_STANDARD 11)

add_executable(json_parser main.cc ds/array.h ds/mystring.cc ds/mystring.h ds/pair.h models/json_value.cc models/json_value.h models/json_null.cc models/json_null.h models/json_string.cc models/json_string.h models/json_boolean.cc models/json_boolean.h models/json_number.cc models/json_number.h models/json_array.cc models/json_array.h models/json_object.cc models/json_object.h parser/json_parser.cc parser/json_parser.h ds/list.cc ds/list.h)