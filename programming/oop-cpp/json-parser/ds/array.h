//
// Created by valio_stoyanov on 5/30/18.
//

#ifndef POLYNOMIAL_ARRAY_H
#define POLYNOMIAL_ARRAY_H

#include <cstddef>
#include <stdexcept>

template<typename T>
class Array {
  size_t size_;
  size_t capacity_;
  T* buffer_;

  T* AllocateNewBuffer(const size_t capacity);
  void EnsureCapacity(const size_t new_elements_count);
  void Copy(T* dest, const size_t dest_size,
            const T* src, const size_t src_size);
  void CopyFrom(const Array&);
 public:
  explicit Array(const size_t initial_capacity = 2);
  Array(const Array&);
  ~Array();

  Array& operator=(const Array&);
  bool operator==(const Array&);
  bool operator!=(const Array&);

  T& operator[](const size_t);
  const T& operator[](const size_t) const;

  void PushBack(const T&);
  void PopBack();
  void Fill(const T&);
  void Clear();

  T& At(const size_t);
  const T& At(const size_t) const;

  T& Front();
  const T& Front() const;

  T& Back();
  const T& Back() const;

  size_t Size() const;
  size_t Capacity() const;

  bool Empty() const;

  class Iterator {
    T* buffer_;
    size_t index_;
   public:
    Iterator(T* buffer_, const size_t index)
        : buffer_(buffer_), index_(index) {};

    Iterator& operator++() {
      ++index_;
      return *this;
    }

    Iterator operator++(int) {
      Iterator result(*this);
      ++index_;
      return result;
    }

    Iterator& operator--() {
      --index_;
      return *this;
    }

    Iterator operator--(int) {
      Iterator result(*this);
      --index_;
      return result;
    }

    bool operator==(const Iterator& other) const {
      return index_ == other.index_;
    }

    bool operator!=(const Iterator& other) const {
      return !(operator==(other));
    }

    T& operator*() {
      return buffer_[index_];
    }
  };

  Iterator Begin() const {
    return Iterator(buffer_, 0);
  }

  Iterator End() const {
    return Iterator(buffer_, size_);
  }
};

template<typename T>
T* Array<T>::AllocateNewBuffer(const size_t capacity) {
  T* buffer;
  try {
    buffer = new T[capacity];
  } catch (const std::bad_alloc&) {
    Clear();
    throw;
  }

  return buffer;
}

template<typename T>
void Array<T>::EnsureCapacity(const size_t new_elements_count) {
  if (size_ + new_elements_count <= capacity_)
    return;
  size_t new_capacity = std::max(size_ + new_elements_count, 2 * capacity_);
  T* new_buffer = AllocateNewBuffer(new_capacity);
  try {
    Copy(new_buffer, new_capacity, buffer_, capacity_);
  } catch (...) {
    delete[] new_buffer;
    throw;
  };
  delete[] buffer_;
  buffer_ = new_buffer;
  capacity_ = new_capacity;
}

template<typename T>
void Array<T>::Copy(T* dest, const size_t dest_size,
                    const T* src, const size_t src_size) {
  if (dest == nullptr || src == nullptr)
    return;
  const size_t copy_size = std::min(dest_size, src_size);
  for (size_t i = 0; i < copy_size; ++i) {
    try {
      dest[i] = src[i];
    } catch (...) {
      Clear();
      throw;
    }
  }
}

template<typename T>
void Array<T>::CopyFrom(const Array& other) {
  Clear();
  EnsureCapacity(other.capacity_);
  size_ = other.size_;
  Copy(buffer_, size_, other.buffer_, size_);
}

template<typename T>
Array<T>::Array(const size_t initial_capacity/* = 2*/)
    : size_(0), capacity_(0), buffer_(nullptr) {
  EnsureCapacity(initial_capacity);
}

template<typename T>
Array<T>::Array(const Array& other)
    : buffer_(nullptr) {
  CopyFrom(other);
}

template<typename T>
Array<T>::~Array() {
  Clear();
}

template<typename T>
Array<T>& Array<T>::operator=(const Array& other) {
  if (this != &other)
    CopyFrom(other);

  return *this;
}

template<typename T>
bool Array<T>::operator==(const Array& other) {
  if (this == &other) return true;
  if (size_ != other.size_) return false;

  for (size_t i = 0; i < size_; ++i)
    if (buffer_[i] != other.buffer_[i]) return false;

  return true;
}

template<typename T>
bool Array<T>::operator!=(const Array& other) {
  return !(*this == other);
}

template<typename T>
T& Array<T>::operator[](const size_t pos) {
  return At(pos);
}

template<typename T>
const T& Array<T>::operator[](const size_t pos) const {
  return At(pos);
}

template<typename T>
void Array<T>::PushBack(const T& val) {
  EnsureCapacity(1);
  buffer_[size_++] = val;
}

template<typename T>
void Array<T>::PopBack() {
  T* res = AllocateNewBuffer(size_);
  try {
    Copy(res, size_ - 1, buffer_, size_);
  } catch (...) {
    delete[] res;
    throw;
  }
  delete[] buffer_;
  buffer_ = res;
  capacity_ = size_--;
}

template<typename T>
void Array<T>::Fill(const T& val) {
  if (size_ > 0) {
    const size_t old_capacity = capacity_;
    Clear();
    EnsureCapacity(old_capacity);
    size_ = 0;
  }

  for (size_t i = 0; i < capacity_; ++i) buffer_[i] = val;
  size_ = capacity_;
}

template<typename T>
void Array<T>::Clear() {
  delete[] buffer_;
  buffer_ = nullptr;
  size_ = 0;
  capacity_ = 0;
}

template<typename T>
T& Array<T>::At(const size_t pos) {
  if (pos >= size_)
    throw std::out_of_range("Array: pos out of boundaries.");

  return buffer_[pos];
}

template<typename T>
const T& Array<T>::At(const size_t pos) const {
  if (pos >= size_)
    throw std::out_of_range("Array: pos out of boundaries.");

  return buffer_[pos];
}

template<typename T>
T& Array<T>::Front() {
  if (Empty())
    throw std::runtime_error("Array: attempt to access element on empty buffer.");

  return buffer_[0];
}

template<typename T>
const T& Array<T>::Front() const {
  if (Empty())
    throw std::runtime_error("Array: attempt to access element on empty buffer.");

  return buffer_[0];
}

template<typename T>
T& Array<T>::Back() {
  if (Empty())
    throw std::runtime_error("Array: attempt to access element on empty buffer.");

  return buffer_[size_ - 1];
}

template<typename T>
const T& Array<T>::Back() const {
  if (Empty())
    throw std::runtime_error("Array: attempt to access element on empty buffer.");

  return buffer_[size_ - 1];
}

template<typename T>
size_t Array<T>::Size() const {
  return size_;
}

template<typename T>
size_t Array<T>::Capacity() const {
  return capacity_;
}

template<typename T>
bool Array<T>::Empty() const {
  return size_ == 0;
}

#endif //POLYNOMIAL_ARRAY_H
