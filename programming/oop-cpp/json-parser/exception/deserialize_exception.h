//
// Created by valio_stoyanov on 6/29/18.
//

#ifndef JSON_PARSER_DESERIALIZE_EXCEPTION_H
#define JSON_PARSER_DESERIALIZE_EXCEPTION_H

#include <stdexcept>
#include "../ds/mystring.h"

class DeserializeException : public std::runtime_error {
  String message_;
 public:
  explicit DeserializeException(const char*);
  explicit DeserializeException(const String&);
  const char* what() const noexcept override;
};

#endif //JSON_PARSER_DESERIALIZE_EXCEPTION_H
