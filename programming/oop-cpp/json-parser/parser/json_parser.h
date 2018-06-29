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

class JsonParser {

  static const char kMinusCh;
  static const char kPlusCh;
  static const char keCh;
  static const char kECh;
  static const char kZeroCh;
  static const char kFloatingPtCh;

  void ThrowIfNull(const char* str, const char* error_msg);

  String ParseKey(const char*);
  JsonValue* ParseValue(const char*&);
  JsonString* ParseString(const char*&);
  JsonValue* ParseNumber(const char*&);
  JsonArray* ParseArray(const char*&);
 public:
  JsonValue* Parse(const char*&);

  class JsonParseException : public std::exception {
    String msg_;
   public:
    explicit JsonParseException(const char* txt = "JsonParser: Failed to parse json.")
        : msg_(txt) {}
    const char* what() const override {
      return msg_.CStr();
    }
  };
};

#endif //JSON_PARSER_JSON_PARSER_H
