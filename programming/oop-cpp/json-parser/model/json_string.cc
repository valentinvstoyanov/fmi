#include "json_string.h"
#include "../util/json_token.h"
#include "../util/cstr.h"
#include "../exception/deserialize_exception.h"

JsonString::JsonString(const char* str)
    : content_(str) {}

JsonString::JsonString(const String& str)
    : content_(str) {}

std::ostream& operator<<(std::ostream& out, const JsonString& json_str) {
  return out << JsonToken::kStrCh << json_str << JsonToken::kStrCh;;
}

void JsonString::Serialize(std::ostream& out, bool pretty, unsigned depth) const {
  out << JsonToken::kStrCh << content_ << JsonToken::kStrCh;
}

JsonString* JsonString::Deserialize(const char*& str) {
  if (*str != JsonToken::kStrCh) {
    String err_msg("Missing opening quotation mark for string value > ");
    err_msg.Append(String(str));
    throw DeserializeException(err_msg);
  }

  const int closing_quot_mark_index = StrIndexOf(str + 1, JsonToken::kStrCh) + 1;
  if (closing_quot_mark_index < 0) {
    String err_msg("Missing closing quotation mark for string value > ");
    err_msg.Append(String(str));
    throw DeserializeException(err_msg);
  }

  String parsed_str(static_cast<size_t>(closing_quot_mark_index - 1));
  for (unsigned i = 1; i < closing_quot_mark_index; ++i)
    parsed_str.PushBack(str[i]);

  str += closing_quot_mark_index + 1;
  return new JsonString(parsed_str);
}


