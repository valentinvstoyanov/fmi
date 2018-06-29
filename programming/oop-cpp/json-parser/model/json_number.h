//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_NUMBER_H
#define JSON_PARSER_JSON_NUMBER_H

#include "json_value.h"
class JsonNumber : public JsonValue {
  union Value {
    long long integer_;
    double real_;
  };

  Value value_;
  bool is_real_;
 public:
  explicit JsonNumber(long long);
  explicit JsonNumber(double);
  bool IsReal() const;
  bool IsInteger() const;
  long long GetInteger() const;
  double GetReal() const;
  void Serialize(std::ostream& out, bool pretty, unsigned depth) const override;
  static JsonNumber* Deserialize(const char*&);
};


#endif //JSON_PARSER_JSON_NUMBER_H
