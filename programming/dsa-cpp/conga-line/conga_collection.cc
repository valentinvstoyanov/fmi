//
// Created by valio_stoyanov on 11/3/18.
//

#include <cassert>
#include <ostream>
#include <iostream>
#include "conga_collection.h"

void CongaCollection::Clear() {
  delete[] conga_lines_;
  size_ = 0;
  capacity_ = 0;
}

void CongaCollection::CopyCongaCollection(const CongaCollection& other) {
  if (other.capacity_ == 0)
    return;
  conga_lines_ = new CongaLine[other.capacity_];
  capacity_ = other.capacity_;
  for (size_t i = 0; i < other.size_; ++i)
    conga_lines_[i] = other.conga_lines_[i];
  size_ = other.size_;
}

CongaCollection::CongaCollection(size_t capacity)
    : size_(0) {
  if (capacity == 0) {
    conga_lines_ = nullptr;
    capacity_ = 0;
  } else {
    conga_lines_ = new CongaLine[capacity];
    capacity_ = capacity;
  }
}

CongaCollection::CongaCollection(const CongaCollection& other)
    : conga_lines_(nullptr), size_(0), capacity_(0) {
  CopyCongaCollection(other);
}

CongaCollection& CongaCollection::operator=(const CongaCollection& other) {
  if (this != &other) {
    Clear();
    CopyCongaCollection(other);
  }
  return *this;
}

CongaCollection::~CongaCollection() {
  delete[] conga_lines_;
}

size_t CongaCollection::Size() const {
  return size_;
}

long CongaCollection::Append(const Student& student, size_t conga_index) {
  if (conga_index >= size_) {
    CongaLine conga_line;
    conga_line.PushBack(student);
    PushBack(conga_line);
    return size_ - 1;
  }
  CongaLine& conga_line = conga_lines_[conga_index];
  if (conga_line.Empty() || student.HasToleranceTo(conga_line.Back())) {
    conga_line.PushBack(student);
    return conga_index;
  }
  return -1;
}

void CongaCollection::RemoveLast(size_t conga_index) {
  assert(conga_index < size_ && "CongaCollection: conga_index out of bounds.");
  CongaLine& conga_line = conga_lines_[conga_index];
  conga_line.PopBack();
  if (conga_line.Empty()) {
    std::swap(conga_lines_[conga_index], conga_lines_[size_ - 1]);
    --size_;
  }
}

void CongaCollection::RemoveFirst(size_t conga_index) {
  assert(conga_index < size_ && "CongaCollection: conga_index out of bounds.");
  CongaLine& conga_line = conga_lines_[conga_index];
  conga_line.PopFront();
  if (conga_line.Empty()) {
    std::swap(conga_lines_[conga_index], conga_lines_[size_ - 1]);
    --size_;
  }
}

void CongaCollection::Remove(const std::string& name, size_t conga_index) {
  assert(conga_index < size_ && "CongaCollection: conga_index out of bounds.");
  CongaLine& conga_line = conga_lines_[conga_index];
  CongaLine result_conga = conga_line.Remove(name);
  if (conga_line.Empty()) {
    if (result_conga.Empty()) {
      std::swap(conga_lines_[conga_index], conga_lines_[size_ - 1]);
      --size_;
    } else {
      conga_line = result_conga;
    }
  } else {
    if (!result_conga.Empty())
      PushBack(result_conga);
  }
}

bool CongaCollection::Merge(size_t conga_idx1, size_t conga_idx2) {
  assert((conga_idx1 < size_ && conga_idx2 < size_) && "CongaCollection: conga_index out of bounds.");
  CongaLine& conga_line1 = conga_lines_[conga_idx1];
  CongaLine& conga_line2 = conga_lines_[conga_idx2];
  if (conga_line2.Front().HasToleranceTo(conga_line1.Back())) {
    conga_line1.Append(conga_line2);
    std::swap(conga_lines_[conga_idx2], conga_lines_[size_ - 1]);
    --size_;
    return true;
  }
  return false;
}

std::ostream& operator<<(std::ostream& out, const CongaCollection& collection) {
  for (size_t i = 0; i < collection.Size(); ++i)
    out << "Line" << i << ": " << collection.conga_lines_[i] << std::endl;
  return out;
}

void CongaCollection::PushBack(const CongaLine& conga_line) {
  if (size_ == capacity_) {
    const size_t new_capacity = capacity_ == 0 ? 2 : 2 * capacity_;
    CongaLine* buff = new CongaLine[new_capacity];
    capacity_ = new_capacity;
    for (size_t i = 0; i < size_; ++i)
      buff[i] = conga_lines_[i];
    delete[] conga_lines_;
    conga_lines_ = buff;
  }
  conga_lines_[size_++] = conga_line;
}

