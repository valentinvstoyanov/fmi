//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_NULL_H
#define JSON_PARSER_JSON_NULL_H

#include <ostream>
#include "json_value.h"

class JsonNull : public JsonValue {
 public:
  JsonNull() = default;
  JsonNull(const JsonNull&) = delete;
  JsonNull& operator=(const JsonNull&) = delete;
  friend std::ostream& operator<<(std::ostream&, const JsonNull&);
};

#endif //JSON_PARSER_JSON_NULL_H
