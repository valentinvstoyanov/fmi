//
// Created by valio_stoyanov on 5/14/18.
//

#include <fstream>
#include "json_parser.h"
#include "../util/cstr.h"
#include "../util/json_token.h"
#include "../model/json_object.h"
#include "../exception/deserialize_exception.h"

void JsonParser::PrintError(const std::runtime_error& error) {
  std::cerr << error.what() << std::endl;
}

JsonValue* JsonParser::Validate(const char* json) {
  try {
    return JsonValue::FromJson(json);
  } catch (const DeserializeException& error) {
    PrintError(error);
    return nullptr;
  }
}

bool JsonParser::CheckValidity(const char* json) {
  return Validate(json) != nullptr;
}

bool JsonParser::CheckValidity(const String& json) {
  return CheckValidity(json.CStr());
}

JsonValue* JsonParser::parseFromFile(const File& file, bool nothrow) {
  if (nothrow) {
    try {
      return parseFromString(file.GetContent(), nothrow);
    } catch (const File::FileException& error) {
      PrintError(error);
      return nullptr;
    }
  }else {
    return parseFromString(file.GetContent(), nothrow);
  }
}

JsonValue* JsonParser::parseFromFile(const char* filename, bool nothrow) {
  return parseFromFile(String(filename), nothrow);
}
JsonValue* JsonParser::parseFromFile(const String& filename, bool nothrow) {
  return parseFromFile(File(filename), nothrow);
}

JsonValue* JsonParser::parseFromString(const char* json, bool nothrow) {
  return nothrow ? Validate(json) : JsonValue::FromJson(json);
}

JsonValue* JsonParser::parseFromString(const String& filename, bool nothrow) {
  return parseFromString(filename.CStr(), nothrow);
}
