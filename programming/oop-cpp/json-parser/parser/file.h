//
// Created by valio_stoyanov on 6/30/18.
//

#ifndef JSON_PARSER_FILE_H
#define JSON_PARSER_FILE_H

#include "../ds/mystring.h"
#include "../model/json_value.h"

class File {
  String name_;
 public:
  explicit File(const char*);
  explicit File(const String&);
  String GetContent() const;
  const String& GetName() const;
  void Save(const JsonValue& json, bool pretty) const;
  bool Exists() const;

  class FileException : public std::runtime_error {
   public:
    explicit FileException(const char* txt) : runtime_error(txt) {}
  };
};

#endif //JSON_PARSER_FILE_H
