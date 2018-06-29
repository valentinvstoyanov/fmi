//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_PARSER_H
#define JSON_PARSER_JSON_PARSER_H

#include "../model/json_value.h"
#include "../ds/mystring.h"
#include "../model/json_string.h"
#include "../model/json_boolean.h"
#include "../model/json_null.h"
#include "../model/json_array.h"
#include "file.h"

class JsonParser {
  static void PrintError(const std::runtime_error&);
  static JsonValue* Validate(const char*);
 public:
  static bool CheckValidity(const char*);
  static bool CheckValidity(const String&);
  static JsonValue* parseFromFile(const File& file, bool nothrow = false);
  static JsonValue* parseFromFile(const char* filename, bool nothrow = false);
  static JsonValue* parseFromFile(const String& filename, bool nothrow = false);
  static JsonValue* parseFromString(const char* json, bool nothrow = false);
  static JsonValue* parseFromString(const String& json, bool nothrow = false);
};

#endif //JSON_PARSER_JSON_PARSER_H
