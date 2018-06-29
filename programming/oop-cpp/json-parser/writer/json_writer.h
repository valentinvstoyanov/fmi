//
// Created by valio_stoyanov on 6/29/18.
//

#ifndef JSON_PARSER_JSON_SERIALIZER_H
#define JSON_PARSER_JSON_SERIALIZER_H

#include "../ds/mystring.h"
#include "../model/json_object.h"
#include "../model/json_array.h"
#include "../model/json_boolean.h"
#include "../model/json_null.h"

class JsonWriter {
  bool pretty_;
 public:
  explicit JsonWriter(bool = false);
  void SetPretty(bool);
  bool IsPretty() const;
  void Write(std::ostream&, const JsonValue&);
};

#endif //JSON_PARSER_JSON_SERIALIZER_H
