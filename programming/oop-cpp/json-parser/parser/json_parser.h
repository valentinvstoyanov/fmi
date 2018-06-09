//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_PARSER_H
#define JSON_PARSER_JSON_PARSER_H

#include "../models/json_value.h"
#include "../ds/mystring.h"
#include "../models/json_string.h"
#include "../models/json_boolean.h"

class JsonParser {
  class StringParseException: std::runtime_error {
   public:
    explicit StringParseException(const char* txt = "Failed to parse string from json.")
        : runtime_error(txt) {}
  };
  class JsonParseException: std::runtime_error {
   public:
    explicit JsonParseException(const char* txt = "Failed to parse json.")
        : runtime_error(txt) {}
  };

  JsonString* ParseString(const String&);
  JsonBoolean* ParseBool(const String&);
 public:
  JsonValue* Parse(const String&);
  JsonValue* Parse(const char*);
};

#endif //JSON_PARSER_JSON_PARSER_H
