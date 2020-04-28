//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include <algorithm>
#include <stdexcept>
#include <iostream>
#include "mystring.h"

void String::EnsureCapacity(const size_t new_elements) {
  if (size_ + new_elements < capacity_)
    return;

  const size_t kNewCapacity = std::max(capacity_ + new_elements + 1, 2 * capacity_ + 1);
  char* old_buff = buffer_;
  buffer_ = new char[kNewCapacity];
  strcpy(buffer_, old_buff);
  delete[] old_buff;
  old_buff = nullptr;
  capacity_ = kNewCapacity;
}

String::String(const char* str/* = ""*/) {
  if (str == nullptr)
    throw std::runtime_error("String: nullptr passed as ctor argument.");
  size_ = strlen(str);
  capacity_ = size_ + 1;
  buffer_ = new char[capacity_];
  strcpy(buffer_, str);
}

String::String(const String& other) {
  size_ = other.size_;
  capacity_ = other.capacity_;
  buffer_ = new char[capacity_];
  strcpy(buffer_, other.buffer_);
}

String::~String() {
  delete[] buffer_;
  buffer_ = nullptr;
}

String& String::operator=(const String& other) {
  if (this != &other) {
    delete[] buffer_;
    size_ = other.size_;
    capacity_ = other.capacity_;
    buffer_ = new char[capacity_];
    strcpy(buffer_, other.buffer_);
  }

  return *this;
}

void String::PushBack(char c) {
  EnsureCapacity(1);
  buffer_[size_++] = c;
  buffer_[size_] = '\0';
}

size_t String::Length() const {
  return size_;
}

bool String::Empty() const {
  return size_ == 0;
}

char& String::At(size_t pos) const {
  if (pos >= size_)
    throw std::out_of_range("String: Index out of boundaries.");

  return buffer_[pos];
}

char& String::At(size_t pos) {
  if (pos >= size_)
    throw std::out_of_range("String: Index out of boundaries.");

  return buffer_[pos];
}

String& String::Append(const String& str) {
  EnsureCapacity(str.size_);
  size_ += str.size_;
  strcat(buffer_, str.buffer_);
  return *this;
}

char& String::Back() {
  if (Empty())
    throw std::runtime_error("String: Back() called on Empty string.");

  return buffer_[size_ - 1];
}

void String::Reverse() {
  for (int i = 0; i < size_ / 2; ++i)
    std::swap(buffer_[i], buffer_[size_ - (i + 1)]);
}

const char& String::Back() const {
  if (Empty())
    throw std::runtime_error("String: Back() called on Empty string.");

  return buffer_[size_ - 1];
}

char& String::Front() {
  if (Empty())
    throw std::runtime_error("String: Front() called on Empty string.");

  return buffer_[0];
}

const char& String::Front() const {
  if (Empty())
    throw std::runtime_error("String: Front() called on Empty string.");

  return buffer_[0];
}

const char* String::CStr() const {
  return buffer_;
}

String String::FromInt(int num) {
  if (num == 0)
    return String("0");

  String result("");
  while (num != 0) {
    int digit = num % 10;
    result.PushBack(digit + '0');
    num /= 10;
  }
  result.Reverse();

  return result;
}

void String::Serialize(std::ostream& out) const {
  if (out.good()) {
    out.write(reinterpret_cast<const char*>(&size_), sizeof(size_t));
    if (size_ > 0) out.write(reinterpret_cast<const char*>(&buffer_), size_);
  }
}

void String::Deserialize(std::istream& in) {
  delete[] buffer_;
  buffer_ = nullptr;
  size_ = 0;
  capacity_ = 0;
  if (in.good()) {
    in.read(reinterpret_cast<char*>(&size_), sizeof(size_t));
    capacity_ = size_ + 1;
    buffer_ = new char[capacity_];
    buffer_[size_] = '\0';
    if (size_ > 0) in.read(reinterpret_cast<char*>(&buffer_), 3);
  }
}

bool String::operator==(const String& other) const {
  return strcmp(buffer_, other.buffer_) == 0;
}

int String::IndexOf(const char c) const {
  for (size_t i = 0; i < size_; ++i)
    if (buffer_[i] == c)
      return static_cast<int>(i);

  return -1;
}

std::ostream& operator<<(std::ostream& out, const String& str) {
  if (str.size_ > 0) out << str.buffer_;
  return out;
}


