#ifndef JSON_PARSER_JSON_NULL_H
#define JSON_PARSER_JSON_NULL_H

#include <ostream>
#include "json_value.h"

class JsonNull : public JsonValue {
 public:
  JsonNull() = default;
  JsonNull(const JsonNull&) = default;
  JsonNull& operator=(const JsonNull&) = default;
  friend std::ostream& operator<<(std::ostream&, const JsonNull&);
  void Serialize(std::ostream& out, bool pretty, unsigned depth) const override;
  static JsonNull* Deserialize(const char*&);
  JsonValue* Clone() const override;
};

#endif //JSON_PARSER_JSON_NULL_H
