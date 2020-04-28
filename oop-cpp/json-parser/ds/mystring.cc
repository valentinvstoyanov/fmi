#include <cstring>
#include <regex>
#include "mystring.h"

void String::EnsureCapacity(size_t new_elements) {
  if (size_ + new_elements <= capacity_)
    return;

  const size_t new_capacity = std::max(capacity_ + new_elements, 2 * capacity_);
  char* old_buffer = buffer_;
  buffer_ = new char[new_capacity + 1];
  strcpy(buffer_, old_buffer);
  delete[] old_buffer;
  old_buffer = nullptr;
  capacity_ = new_capacity;
}

String::String(const char* str)
    : size_(0), capacity_(0), buffer_(nullptr) {
  if (str) {
    size_ = strlen(str);
    capacity_ = size_ + 1;
    buffer_ = new char[capacity_ + 1];
    strcpy(buffer_, str);
  }
}

String::String(size_t initial_capacity)
    : size_(0), capacity_(initial_capacity), buffer_(nullptr) {
  buffer_ = new char[capacity_ + 1];
  buffer_[capacity_] = '\0';
}

String::String(const String& other)
    : size_(other.size_), capacity_(other.capacity_), buffer_(nullptr) {
  if (other.buffer_) {
    buffer_ = new char[capacity_ + 1];
    strcpy(buffer_, other.buffer_);
  }
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
    buffer_ = new char[capacity_ + 1];
    strcpy(buffer_, other.buffer_);
  }

  return *this;
}

std::ostream& operator<<(std::ostream& os, const String& str) {
  if (str.size_ > 0)
    os << str.buffer_;

  return os;
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

void String::Clear() {
  delete[] buffer_;
  buffer_ = nullptr;
  size_ = 0;
  capacity_ = 0;
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

bool String::operator==(const String& other) const {
  return strcmp(buffer_, other.buffer_) == 0;
}

bool String::operator!=(const String& other) const {
  return !(*this == other);
}

bool String::operator<(const String& other) const {
  return strcmp(buffer_, other.buffer_) < 0;
}

bool String::operator>(const String& other) const {
  return strcmp(buffer_, other.buffer_) > 0;
}

bool String::operator<=(const String& other) const {
  return strcmp(buffer_, other.buffer_) <= 0;
}

bool String::operator>=(const String& other) const {
  return strcmp(buffer_, other.buffer_) >= 0;
}

char& String::operator[](const size_t pos) {
  if (pos >= size_)
    throw std::out_of_range("String: Index out of boundaries.");

  return buffer_[pos];
}

char& String::operator[](const size_t pos) const {
  if (pos >= size_)
    throw std::out_of_range("String: Index out of boundaries.");

  return buffer_[pos];
}

String String::Substr(size_t pos, size_t n) const {
  if (pos >= size_)
    throw std::out_of_range("String: Index out of boundaries.");

  char* substr = new char[n + 1];
  strncpy(substr, buffer_ + pos, n);
  substr[n] = '\0';
  String substr_obj(substr);
  delete[] substr;
  return substr_obj;
}

int String::IndexOf(char c) const {
  for (int i = 0; i < size_; ++i)
    if (buffer_[i] == c)
      return i;

  return -1;
}

bool String::Contains(char c) const {
  return IndexOf(c) >= 0;
}

bool String::IsWhiteSpace(char c) {
  static const String kWhitespaceCharacters = String(" \n\r\t\v\f");
  return kWhitespaceCharacters.Contains(c);
}

const char* String::CStr() const {
  return buffer_;
}

Array<String> String::Split(char delim) const {
  Array<String> res;
  size_t curr_pos = 0;
  for (size_t i = 0; i < size_; ++i) {
    if (buffer_[i] == delim) {
      res.PushBack(Substr(curr_pos, i - curr_pos));
      curr_pos = i + 1;
    }
  }
  res.PushBack(Substr(curr_pos, size_ - curr_pos));

  return res;
}

String::String(): String(2) {}

