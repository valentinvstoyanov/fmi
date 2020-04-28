//
// Created by valio_stoyanov on 5/24/18.
//

#include "post_array.h"

void PostArray::EnsureCapacity(const size_t new_elements) {
  if (size_ + new_elements <= capacity_)
    return;

  Post** old_arr = arr_;
  const size_t new_capacity = std::max(new_elements + size_, 2 * capacity_);
  arr_ = new Post*[new_capacity];

  for (size_t i = 0; i < size_; ++i)
    arr_[i] = old_arr[i];

  delete[] old_arr;
  old_arr = nullptr;

  for (size_t i = size_; i < new_capacity; ++i)
    arr_[i] = nullptr;

  capacity_ = new_capacity;
}

PostArray::PostArray(const size_t initial_capacity)
    : size_(0), capacity_(0), arr_(nullptr) {
  EnsureCapacity(initial_capacity);
}

PostArray::PostArray(const PostArray& other)
    :size_(0), capacity_(0), arr_(nullptr) {
  EnsureCapacity(other.capacity_);
  size_ = other.size_;
  for (int i = 0; i < size_; ++i)
    arr_[i] = other.arr_[i]->Clone();
}

PostArray::~PostArray() {
  Clear();
}

PostArray& PostArray::operator=(const PostArray& other) {
  if (this != &other) {
    Clear();
    EnsureCapacity(other.capacity_);
    size_ = other.size_;
    for (size_t i = 0; i < size_; ++i)
      arr_[i] = other.arr_[i]->Clone();
  }

  return *this;
}

void PostArray::Clear() {
  for (int i = 0; i < size_; ++i) {
    delete arr_[i];
    arr_[i] = nullptr;
  }
  delete[] arr_;
  arr_ = nullptr;
  size_ = 0;
  capacity_ = 0;
}

void PostArray::PushBack(const Post& post) {
  EnsureCapacity(1);
  arr_[size_++] = post.Clone();
}

void PostArray::PopBack() {
  if (!Empty())
    removeAt(size_ - 1);
}

Post& PostArray::At(const size_t pos) {
  return *arr_[pos];
}

const Post& PostArray::At(const size_t pos) const {
  return *arr_[pos];
}

const Post& PostArray::Front() const {
  return *arr_[0];
}

Post& PostArray::Front() {
  return *arr_[0];
}

const Post& PostArray::Back() const {
  return *arr_[size_ - 1];
}

Post& PostArray::Back() {
  return *arr_[size_ - 1];
}

bool PostArray::Empty() const {
  return size_ == 0;
}

size_t PostArray::Size() const {
  return size_;
}

size_t PostArray::Capacity() const {
  return capacity_;
}

void PostArray::removeAt(size_t pos) {
  if (pos < 0 || pos >= size_)
    throw std::out_of_range("PostArray: \"void removeAt(size_t)\" pos out of boundaries");

  Post** old_arr = arr_;
  arr_ = new Post*[size_];

  for (size_t i = 0; i < pos; ++i)
    arr_[i] = old_arr[i];
  for (size_t i = pos + 1; i < size_; ++i)
    arr_[i - 1] = old_arr[i];

  capacity_ = size_--;
}
