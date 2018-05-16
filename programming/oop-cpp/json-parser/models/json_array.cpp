//
// Created by valio_stoyanov on 5/14/18.
//

#include <algorithm>
#include "json_array.h"

void JsonArray::ensure_capacity(const size_t new_elements) {
    if (size_ + new_elements <= capacity_)
        return;

    JsonValue** old_values = values_;
    size_t new_capacity = std::max(2 * capacity_, size_ + new_elements);
    values_ = new JsonValue*[new_capacity];
    for (int i = 0; i < size_; ++i)
        values_[i] = old_values[i];

    delete[] old_values;
    capacity_ = new_capacity;
}

explicit JsonArray::JsonArray(const size_t initial_capacity = 1)
: size_(0), capacity_(initial_capacity), values_(nullptr)
{
    if (capacity_ != 0)
        values_ = new JsonValue*[capacity_];
}

JsonArray::JsonArray(const JsonArray& other)
: size_(other.size_), capacity_(other.capacity_), values_(nullptr)
{
    if (capacity_!= 0) {
        values_ = new JsonValue*[capacity_];
        for (int i = 0; i < size_; ++i)
            values_[i] = other.values_[i];
    }
}

JsonArray::~JsonArray() {
    for (int i = 0; i < size_; ++i) {
        delete values_[i];
        values_[i] = nullptr;
    }

    delete[] values_;
    values_ = nullptr;
}

JsonArray &JsonArray::operator=(const JsonArray& other) {
    if (this != &other) {
        for (int i = 0; i < size_; ++i)
            delete values_[i];
        delete[] values_;

        size_ = other.size_;
        capacity_ = other.capacity_;

        if (capacity_ != 0) {
            values_ = new JsonValue*[capacity_];
            for (int i = 0; i < size_; ++i)
                values_[i] = other.values_[i];
        }
    }

    return *this;
}

size_t JsonArray::size() const {
    return size_;
}

bool JsonArray::empty() const {
    return size_ == 0;
}




