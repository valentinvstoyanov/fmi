#ifndef JSON_PARSER_JSONBOOLEAN_H
#define JSON_PARSER_JSONBOOLEAN_H

#include <ostream>
#include "json_value.h"

class JsonBoolean : public JsonValue {
  bool value_;
 public:
  explicit JsonBoolean(bool);
  friend std::ostream& operator<<(std::ostream&, const JsonBoolean&);
  void Serialize(std::ostream& out, bool pretty, unsigned depth) const override;
  static JsonBoolean* Deserialize(const char*&);
};

#endif //JSON_PARSER_JSONBOOLEAN_H
