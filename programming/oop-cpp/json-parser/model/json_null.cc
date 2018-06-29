//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_null.h"
#include "../util/json_token.h"

std::ostream& operator<<(std::ostream& out, const JsonNull&) {
  return out << JsonToken::kNullStr;
}

void JsonNull::Serialize(std::ostream& out, bool pretty, unsigned depth) const {
  out << JsonToken::kNullStr;
}