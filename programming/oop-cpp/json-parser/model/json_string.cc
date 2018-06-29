//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_string.h"
#include "../util/json_token.h"

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


