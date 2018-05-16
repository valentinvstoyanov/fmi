//
// Created by valio_stoyanov on 5/16/18.
//

#include <algorithm>
#include <stdexcept>
#include "user_array.h"

void UserArray::ensure_capacity(const size_t new_elements) {
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
: size_(0), capacity_(initial_capacity), arr_(nullptr)
{
    if (capacity_ > 0)
        arr_ = new User[capacity_];
}

UserArray::UserArray(const UserArray& other)
: size_(other.size_), capacity_(other.capacity_), arr_(nullptr)
{
    if (capacity_> 0) {
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

void UserArray::clear() {
    delete[] arr_;
    arr_ = nullptr;
    size_ = 0;
    capacity_ = 0;
}

void UserArray::push_back(const User& element) {
    ensure_capacity(1);
    arr_[size_++] = element;
}

void UserArray::pop_back() {
    if (!empty()) {
        User *old_arr = arr_;
        delete[] arr_;
        arr_ = new User[capacity_];
        size_--;
        for (int i = 0; i < size_; ++i)
            arr_[i] = old_arr[i];
    }
}

bool UserArray::empty() const {
    return size_ == 0;
}

size_t UserArray::size() const {
    return size_;
}

size_t UserArray::capacity() const {
    return capacity_;
}

User& UserArray::at(const size_t pos) {
    if (pos < 0 || pos >= size_)
        throw std::out_of_range("UserArray: \" User& at(const size_t);\" called with argument out of boundaries.");

    return arr_[pos];
}

const User& UserArray::at(const size_t pos) const {
    if (pos < 0 || pos >= size_)
        throw std::out_of_range("UserArray: \"const User& at(const size_t) const;\" called with argument out of boundaries.");

    return arr_[pos];
}

const User& UserArray::front() const {
    if (empty())
        throw std::runtime_error("UserArray: \"const User& front() const\" called on empty array.");

    return arr_[0];
}

User& UserArray::front() {
    if (empty())
        throw std::runtime_error("UserArray: \"User& front()\" called on empty array.");

    return arr_[0];
}

const User& UserArray::back() const {
    if (empty())
        throw std::runtime_error("UserArray: \"const User& back() const\" called on empty array.");

    return arr_[size_ - 1];
}

User& UserArray::back() {
    if (empty())
        throw std::runtime_error("UserArray: \"User& back()\" called on empty array.");

    return arr_[size_ - 1];
}
