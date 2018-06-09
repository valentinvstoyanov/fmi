//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_string.h"

JsonString::JsonString(const char* str)
    : content_(str) {}

JsonString::JsonString(const String& str)
    : content_(str) {}

std::ostream& operator<<(std::ostream& out, const JsonString& json_string) {
  return out << json_string.content_;
}


