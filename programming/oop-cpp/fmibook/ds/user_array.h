//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_USER_ARRAY_H
#define FMIBOOK_USER_ARRAY_H

#include <cstddef>
#include "../data/model/user.h"

class UserArray {
  User* arr_;
  size_t size_;
  size_t capacity_;
  void EnsureCapacity(const size_t);
 public:
  explicit UserArray(const size_t = 1);
  UserArray(const UserArray&);
  ~UserArray();
  UserArray& operator=(const UserArray&);
  void Clear();
  void PushBack(const User&);
  void PopBack();
  User& At(const size_t);
  const User& At(const size_t) const;
  const User& Front() const;
  User& Front();
  const User& Back() const;
  User& Back();
  void removeAt(size_t);
  bool Empty() const;
  size_t Size() const;
  size_t Capacity() const;
};

#endif //FMIBOOK_USER_ARRAY_H
