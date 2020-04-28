//
// Created by valio_stoyanov on 5/16/18.
//

#include <algorithm>
#include <stdexcept>
#include "user_array.h"

void UserArray::EnsureCapacity(const size_t new_elements) {
  if (new_elements + size_ <= capacity_)
    return;

  User* old_arr = arr_;
  const size_t new_capacity = std::max(new_elements + size_, 2 * capacity_);
  arr_ = new User[new_capacity];
  for (int i = 0; i < size_; ++i)
    arr_[i] = old_arr[i];
  delete[] old_arr;
  capacity_ = new_capacity;
}

UserArray::UserArray(const size_t initial_capacity)
    : size_(0), capacity_(initial_capacity), arr_(nullptr) {
  if (capacity_ > 0)
    arr_ = new User[capacity_];
}

UserArray::UserArray(const UserArray& other)
    : size_(other.size_), capacity_(other.capacity_), arr_(nullptr) {
  if (capacity_ > 0) {
    arr_ = new User[capacity_];
    for (int i = 0; i < size_; ++i)
      arr_[i] = other.arr_[i];
  }
}

UserArray::~UserArray() {
  delete[] arr_;
  arr_ = nullptr;
}

UserArray& UserArray::operator=(const UserArray& other) {
  if (this != &other) {
    delete[] arr_;
    arr_ = nullptr;
    size_ = other.size_;
    capacity_ = other.capacity_;
    if (capacity_ > 0) {
      arr_ = new User[capacity_];
      for (int i = 0; i < size_; ++i)
        arr_[i] = other.arr_[i];
    }
  }

  return *this;
}

void UserArray::Clear() {
  delete[] arr_;
  arr_ = nullptr;
  size_ = 0;
  capacity_ = 0;
}

void UserArray::PushBack(const User& element) {
  EnsureCapacity(1);
  arr_[size_++] = element;
}

void UserArray::PopBack() {
  if (!Empty()) {
    User* old_arr = arr_;
    delete[] arr_;
    arr_ = new User[capacity_];
    size_--;
    for (int i = 0; i < size_; ++i)
      arr_[i] = old_arr[i];
  }
}

bool UserArray::Empty() const {
  return size_ == 0;
}

size_t UserArray::Size() const {
  return size_;
}

size_t UserArray::Capacity() const {
  return capacity_;
}

User& UserArray::At(const size_t pos) {
  if (pos < 0 || pos >= size_)
    throw std::out_of_range("UserArray: \" User& At(const size_t);\" called with argument out of boundaries.");

  return arr_[pos];
}

const User& UserArray::At(const size_t pos) const {
  if (pos < 0 || pos >= size_)
    throw std::out_of_range("UserArray: \"const User& At(const size_t) const;\" called with argument out of boundaries.");

  return arr_[pos];
}

const User& UserArray::Front() const {
  if (Empty())
    throw std::runtime_error("UserArray: \"const User& Front() const\" called on Empty array.");

  return arr_[0];
}

User& UserArray::Front() {
  if (Empty())
    throw std::runtime_error("UserArray: \"User& Front()\" called on Empty array.");

  return arr_[0];
}

const User& UserArray::Back() const {
  if (Empty())
    throw std::runtime_error("UserArray: \"const User& Back() const\" called on Empty array.");

  return arr_[size_ - 1];
}

User& UserArray::Back() {
  if (Empty())
    throw std::runtime_error("UserArray: \"User& Back()\" called on Empty array.");

  return arr_[size_ - 1];
}

void UserArray::removeAt(const size_t pos) {
  if (pos < 0 || pos >= size_)
    throw std::out_of_range("UserArray: \"removeAt(const size_t)\" called with argument out of boundaries.");

  User* old_arr = arr_;
  arr_ = new User[size_];
  for (size_t i = 0; i < pos; ++i)
    arr_[i] = old_arr[i];
  for (size_t i = pos + 1; i < size_; ++i)
    arr_[i - 1] = old_arr[i];
  delete[] old_arr;
  capacity_ = size_--;
}
