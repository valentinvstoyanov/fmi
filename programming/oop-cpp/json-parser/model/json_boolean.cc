#include <cstring>
#include "json_boolean.h"
#include "../util/json_token.h"
#include "../exception/deserialize_exception.h"

JsonBoolean::JsonBoolean(const bool value)
    : value_(value) {}

std::ostream& operator<<(std::ostream& out, const JsonBoolean& json_bool) {
  return out << (json_bool.value_ ? JsonToken::kTrueStr : JsonToken::kFalseStr);
}
void JsonBoolean::Serialize(std::ostream& out,
                            bool pretty,
                            unsigned depth) const {
  out << (value_ ? JsonToken::kTrueStr : JsonToken::kFalseStr);
}

JsonBoolean* JsonBoolean::Deserialize(const char*& str) {
  if (strncmp(str, JsonToken::kTrueStr, 4) == 0) {
    str += 4;
    return new JsonBoolean(true);
  }

  if (strncmp(str, JsonToken::kFalseStr, 5) == 0) {
    str += 5;
    return new JsonBoolean(false);
  }

  String err_msg("Unknown value. Check for missing quote or boolean value typo > ");
  err_msg.Append(String(str));
  throw DeserializeException(err_msg);
}
