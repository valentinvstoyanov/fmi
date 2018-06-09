//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_boolean.h"

JsonBoolean::JsonBoolean(const bool value)
    : value_(value) {}

std::ostream& operator<<(std::ostream& out, const JsonBoolean& json_bool) {
  return out << (json_bool.value_ ? "true" : "false");
}

