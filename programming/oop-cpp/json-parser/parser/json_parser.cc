//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_parser.h"
#include "../models/json_null.h"

JsonString* JsonParser::ParseString(const String& str) {
  if (str.Front() != '\"')
    throw StringParseException("ParseString: Not a string.");

  String extracted_str("");
  //TODO: If it doesnt have /" what should I do. Maybe throw an exception.
  for (size_t i = 1; i < str.Length() && str[i] != '\"'; ++i)
    extracted_str.PushBack(str[i]);

  return new JsonString(extracted_str);
}

JsonBoolean* JsonParser::ParseBool(const String& str) {
  if (str.Length() < 4 || str.Front() != 'f' || str.Front() != 't') {}
    //throw

  String extracted_str = str.Substr(0, 4);
  if (extracted_str == String("fals") && str[4] == 'e')
    return new JsonBoolean(false);
  if (extracted_str == String("true"))
    return new JsonBoolean(true);

  //throw
}

JsonValue* JsonParser::Parse(const String& json) {
  if (json.Front() == '\"')
    return ParseString(json);
  if (json.Front() == 'f' || json.Front() == 't')
    return ParseBool(json);

  throw JsonParseException("Unknown type");
}

JsonValue* JsonParser::Parse(const char* json) {
  return Parse(String(json));
}
