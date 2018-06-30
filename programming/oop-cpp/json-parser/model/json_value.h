#ifndef JSON_PARSER_JSON_VALUE_H
#define JSON_PARSER_JSON_VALUE_H

#include <ostream>
#include "../ds/mystring.h"

class JsonValue {
 protected:
  void WritePrettyLine(std::ostream& out, unsigned depth) const;
 public:
  virtual ~JsonValue() = default;
  virtual void Serialize(std::ostream& out,
                         bool pretty,
                         unsigned depth) const = 0;
  void Serialize(std::ostream& out, bool pretty) const;
  static JsonValue* FromJson(const char*&);
};

#endif //JSON_PARSER_JSON_VALUE_H
