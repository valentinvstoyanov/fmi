//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_parser.h"
#include "../util/cstr.h"
#include "../util/json_token.h"
#include "../model/json_object.h"

const char kMinusCh = '-';
const char kPlusCh = '+';
const char keCh = 'e';
const char kECh = 'E';
const char JsonParser::kFloatingPtCh = '.';
const char JsonParser::kZeroCh = '0';


void JsonParser::ThrowIfNull(const char* str, const char* error_msg) {
  if (!str) throw JsonParseException(error_msg);
}

String JsonParser::ParseKey(const char* str) {
  if (*str != JsonToken::kStrCh)
    throw JsonParseException("JsonParser: Missing opening quotation mark for key name.");

  const int closing_quot_mark_index = StrIndexOf(str + 1, JsonToken::kStrCh) + 1;
  if (closing_quot_mark_index < 0)
    throw JsonParseException("JsonParser: Missing closing quotation mark for key name.");

  String result(static_cast<size_t>(closing_quot_mark_index - 1));
  for (unsigned i = 1; i < closing_quot_mark_index; ++i)
    result.PushBack(str[i]);

  return result;
}

JsonValue* JsonParser::ParseValue(const char*& str) {
  str = StrSkipWhiteSpace(str);

  if (*str == JsonToken::kStrCh) {
    return ParseString(str);
  } else if (*str == JsonToken::kBegArrCh) {
    return ParseArray(str);
  } else if (*str == '-' || IsDigit(*str)) {
    return ParseNumber(str);
  } else if (strncmp(str, JsonToken::kNullStr, 4) == 0) {
    str += 4;
    return new JsonNull();
  } else if (strncmp(str, JsonToken::kTrueStr, 4) == 0) {
    str += 4;
    return new JsonBoolean(true);
  } else if (strncmp(str, JsonToken::kFalseStr, 5) == 0) {
    str += 5;
    return new JsonBoolean(false);
  } else if (*str== JsonToken::kBegObjCh) {
    return Parse(str);
  }

  throw JsonParseException("JsonParser: Unknown value type.");
}

JsonString* JsonParser::ParseString(const char*& str) {
  if (*str != JsonToken::kStrCh)
    throw JsonParseException("JsonParser: Missing opening quotation mark for string value.");

  const int closing_quot_mark_index = StrIndexOf(str + 1, JsonToken::kStrCh) + 1;
  if (closing_quot_mark_index < 0)
    throw JsonParseException("JsonParser: Missing closing quotation mark for string value.");

  String parsed_str(static_cast<size_t>(closing_quot_mark_index - 1));
  for (unsigned i = 1; i < closing_quot_mark_index; ++i)
    parsed_str.PushBack(str[i]);

  str += closing_quot_mark_index + 1;

  return new JsonString(parsed_str);
}

JsonValue* JsonParser::ParseNumber(const char*& str) {
  throw std::runtime_error("Not implemented.");
}

JsonArray* JsonParser::ParseArray(const char*& str) {
  if (*str != JsonToken::kBegArrCh)
    throw JsonParseException("JsonParser: Missing opening array bracket.");
  JsonArray* json_arr = new JsonArray();

  if (*StrSkipWhiteSpace(str + 1) == JsonToken::kEndArrCh) {
    ++str;
    return json_arr;
  }

  do {
    ++str;
    str = StrSkipWhiteSpace(str);
    JsonValue* value = ParseValue(str);
    json_arr->PushBack(value);
    str = StrSkipWhiteSpace(str);
  } while (*str == JsonToken::kValueSeparatorCh);

  str = StrSkipWhiteSpace(str);

  if (*str != JsonToken::kEndArrCh)
    throw JsonParseException("JsonParser: Missing closing array bracket.");

  ++str;

  return json_arr;
}

JsonValue* JsonParser::Parse(const char*& str) {
  str = StrSkipWhiteSpace(str);
  if (*str != JsonToken::kBegObjCh)
    throw JsonParseException("JsonParser: Missing opening object bracket.");
  JsonObject* json_obj = new JsonObject;

  if (*StrSkipWhiteSpace(str + 1) == JsonToken::kEndObjCh) {
    ++str;
    return json_obj;
  }

  do {
    ++str;
    str = StrSkipWhiteSpace(str);
    String key = ParseKey(str);
    str += key.Length() + 2;
    str = StrSkipWhiteSpace(str);
    if (*str != JsonToken::kColonSeparatorCh)
      throw JsonParseException("JsonParser: Missing separator between key and value.");
    ++str;
    JsonValue* value = ParseValue(str);
    json_obj->PushBack(Pair<String, JsonValue*>(key, value));
    str = StrSkipWhiteSpace(str);
  } while (*str == JsonToken::kValueSeparatorCh);

  str = StrSkipWhiteSpace(str);

  if (*str != JsonToken::kEndObjCh)
    throw JsonParseException("JsonParser: Missing closing object bracket.");

  ++str;

  return json_obj;
}
