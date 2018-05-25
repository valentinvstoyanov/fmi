//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_MYSTRING_H
#define FMIBOOK_MYSTRING_H

#include <cstddef>
#include <ostream>
#include <istream>

class String {
  char* buffer_;
  size_t size_;
  size_t capacity_;
  void EnsureCapacity(const size_t);
 public:
  explicit String(const char* = nullptr);
  String(const String&);
  ~String();
  String& operator=(const String&);
  bool operator==(const String& other) const;
  friend std::ostream& operator<<(std::ostream&, const String&);
  void PushBack(char);
  String& Append(const String&);
  void Clear();
  char& At(size_t);
  char& Back();
  char& Front();
  void Reverse();
  const char* CStr() const;
  const char& Front() const;
  const char& Back() const;
  size_t Length() const;
  bool Empty() const;
  int IndexOf(const char) const;
  char& At(size_t) const;
  void Serialize(std::ostream&) const;
  void Deserialize(std::istream&);
  static String FromInt(int);
};

#endif //FMIBOOK_MYSTRING_H
