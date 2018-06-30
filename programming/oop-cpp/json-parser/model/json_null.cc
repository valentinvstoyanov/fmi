#include <cstring>
#include "json_null.h"
#include "../util/json_token.h"
#include "../exception/deserialize_exception.h"

std::ostream& operator<<(std::ostream& out, const JsonNull&) {
  return out << JsonToken::kNullStr;
}

void JsonNull::Serialize(std::ostream& out, bool pretty, unsigned depth) const {
  out << JsonToken::kNullStr;
}

JsonNull* JsonNull::Deserialize(const char*& str) {
  if (strncmp(str, JsonToken::kNullStr, 4) == 0) {
    str += 4;
    return new JsonNull;
  }

  String err_msg("Null value expected but found > ");
  err_msg.Append(String(str));
  throw DeserializeException(err_msg);
}
