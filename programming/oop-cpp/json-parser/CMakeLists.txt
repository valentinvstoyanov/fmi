cmake_minimum_required(VERSION 3.10)
project(json_parser)

set(CMAKE_CXX_STANDARD 11)

add_executable(json_parser main.cc ds/array.h ds/mystring.cc ds/mystring.h ds/pair.h model/json_value.cc model/json_value.h model/json_null.cc model/json_null.h model/json_string.cc model/json_string.h model/json_boolean.cc model/json_boolean.h model/json_number.cc model/json_number.h model/json_array.cc model/json_array.h model/json_object.cc model/json_object.h parser/json_parser.cc parser/json_parser.h util/cstr.h util/cstr.cc util/json_token.cc util/json_token.h exception/deserialize_exception.cc exception/deserialize_exception.h parser/file.cc parser/file.h)