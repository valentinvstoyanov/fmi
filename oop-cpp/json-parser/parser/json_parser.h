#ifndef JSON_PARSER_JSON_PARSER_H
#define JSON_PARSER_JSON_PARSER_H

#include "../model/json_value.h"
#include "../ds/mystring.h"
#include "../model/json_string.h"
#include "../model/json_boolean.h"
#include "../model/json_null.h"
#include "../model/json_array.h"
#include "file.h"
#include "../model/json_object.h"

class JsonParser {
  static void PrintError(const std::runtime_error&);
  static JsonValue* Validate(const char*);
 public:
  static bool CheckValidity(const char* json);
  static bool CheckValidity(const String& json);

  static JsonValue* ParseFromFile(const File& file, bool nothrow = false);
  static JsonValue* ParseFromFile(const char* filename, bool nothrow = false);
  static JsonValue* ParseFromFile(const String& filename, bool nothrow = false);

  static JsonValue* ParseFromString(const char* json, bool nothrow = false);
  static JsonValue* ParseFromString(const String& json, bool nothrow = false);

  static void WriteToFile(const JsonValue& json,
                          const char* filename,
                          bool pretty = false);
  static void WriteToFile(const JsonValue& json,
                          const String& filename,
                          bool pretty = false);
  static void WriteToFile(const JsonValue& json,
                          const File& file,
                          bool pretty = false);

  static void PrintToStdin(const JsonValue& json, bool pretty = false);

  static JsonObject* Find(Array<String> keys, const JsonObject& obj);
};

#endif //JSON_PARSER_JSON_PARSER_H
