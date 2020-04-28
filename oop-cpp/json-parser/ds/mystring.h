#ifndef JSON_PARSER_STRING_H
#define JSON_PARSER_STRING_H

#include <ostream>
#include "array.h"

class String {
  char* buffer_;
  size_t size_;
  size_t capacity_;
  void EnsureCapacity(size_t);
 public:
  String();
  explicit String(const char*);
  explicit String(size_t initial_capacity);
  String(const String&);
  ~String();

  String& operator=(const String&);
  friend std::ostream& operator<<(std::ostream&, const String&);
  bool operator==(const String& other) const;
  bool operator!=(const String& other) const;
  bool operator<(const String& other) const;
  bool operator>(const String& other) const;
  bool operator<=(const String& other) const;
  bool operator>=(const String& other) const;
  char& operator[](const size_t) ;
  char& operator[](const size_t) const;

  void PushBack(char);
  String& Append(const String&);
  const char* CStr() const;
  String Substr(size_t pos, size_t n) const;
  Array<String> Split(char delim) const;
  int IndexOf(char) const;
  bool Contains(char) const;
  void Clear();
  char& At(size_t);
  char& Back();
  char& Front();
  const char& Front() const;
  const char& Back() const;
  char& At(size_t) const;
  size_t Length() const;
  bool Empty() const;

  static bool IsWhiteSpace(char);
};

#endif //JSON_PARSER_STRING_H
