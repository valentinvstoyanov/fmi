//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include <algorithm>
#include <stdexcept>
#include "mystring.h"

void String::ensure_capacity(const size_t new_elements) {
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
        : size_(0), capacity_(0), buffer_(nullptr)
{
    if (str) {
        size_ = strlen(str);
        capacity_ = size_ + 1;
        buffer_ = new char[capacity_ + 1];
        strcpy(buffer_, str);
    }
}

String::String(const String& other)
        : size_(other.size_), capacity_(other.capacity_), buffer_(nullptr)
{
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

void String::push_back(char c) {
    ensure_capacity(1);
    buffer_[size_++] = c;
    buffer_[size_] = '\0';
}

size_t String::length() const {
    return size_;
}

bool String::empty() const {
    return size_ == 0;
}

char& String::at(const size_t pos) const {
    if (pos >= size_)
        throw std::out_of_range("String: Index out of boundaries.");

    return buffer_[pos];
}

char& String::at(const size_t pos) {
    if (pos >= size_)
        throw std::out_of_range("String: Index out of boundaries.");

    return buffer_[pos];
}

void String::clear() {
    delete[] buffer_;
    buffer_ = nullptr;
    size_ = 0;
    capacity_ = 0;
}

String& String::append(const String& str) {
    ensure_capacity(str.size_);
    size_ += str.size_;
    strcat(buffer_, str.buffer_);

    return *this;
}

char& String::back() {
    if (empty())
        throw std::runtime_error("String: back() called on empty string.");

    return buffer_[size_ - 1];
}

void String::reverse() {
    for (int i = 0; i < size_ / 2; ++i)
        std::swap(buffer_[i], buffer_[size_- (i + 1)]);
}

const char& String::back() const {
    if (empty())
        throw std::runtime_error("String: back() called on empty string.");

    return buffer_[size_ - 1];
}

char& String::front() {
    if (empty())
        throw std::runtime_error("String: front() called on empty string.");

    return buffer_[0];
}

const char& String::front() const {
    if (empty())
        throw std::runtime_error("String: front() called on empty string.");

    return buffer_[0];
}

const char* String::c_str() const {
    return buffer_;
}

String String::fromInt(int num) {
    String result("");

    while (num != 0) {
        int digit = num % 10;
        result.push_back(digit + '0');
        num /= 10;
    }
    result.reverse();

    return result;
}

void String::serialize(std::ostream& out) const {
    if (out.good()) {
        out.write(reinterpret_cast<const char*>(&size_), sizeof(size_t));
        out.write(buffer_, size_);
    }
}

void String::deserialize(std::istream& in) {
    clear();
    if (in.good()) {
        size_t read_size;
        in.read(reinterpret_cast<char*>(&read_size), sizeof(size_t));
        if (read_size > 0) {
            ensure_capacity(read_size);
            size_ = read_size;
            in.read(buffer_, read_size);
        }
    }
}

