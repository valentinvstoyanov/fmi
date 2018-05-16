//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_object.h"

void JsonObject::ensure_capacity(const size_t new_elements) {
    if (size_ + new_elements <= capacity_)
        return;

    Pair* old_pairs = pairs_;
    size_t new_capacity = std::max(2 * capacity_, size_ + new_elements);
    pairs_ = new Pair[new_capacity];
    for (int i = 0; i < size_; ++i)
        pairs_[i] = old_pairs[i];
    delete[] old_pairs;
    old_pairs = nullptr;
    capacity_ = new_capacity;
}

JsonObject::JsonObject(const size_t initial_capacity)
: size_(0), capacity_(initial_capacity), pairs_(nullptr)
{
    if (capacity_ != 0)
        pairs_ = new Pair[capacity_];
}

JsonObject::JsonObject(const JsonObject& other)
: size_(other.size_), capacity_(other.capacity_), pairs_(nullptr)
{
    if (capacity_ != 0) {
        pairs_ = new Pair[capacity_];
        for (int i = 0; i < size_; ++i)
            pairs_[i] = other.pairs_[i];
    }
}

JsonObject::~JsonObject() {
    delete[] pairs_;
    pairs_ = nullptr;
}

JsonObject& JsonObject::operator=(const JsonObject& other) {
    if (this != &other) {
        size_ = other.size_;
        capacity_ = other.capacity_;
        delete[] pairs_;
        pairs_ = new Pair[capacity_];
        for (int i = 0; i < size_; ++i)
            pairs_[i] = other.pairs_[i];
    }

    return *this;
}

void JsonObject::push_back(const Pair& pair) {
    ensure_capacity(1);
    pairs_[size_++] = pair;
}

void JsonObject::remove(const size_t pos) {
    if(pos < 0 || pos >= size_)
        throw std::out_of_range("JsonObject remove(const size_t) called with out of boundaries pos");

    Pair* old_pairs = pairs_;
    pairs_ = new Pair[capacity_];
    for (size_t i = 0; i < pos; ++i)
        pairs_[i] = old_pairs[i];
    for (size_t i = pos + 1; i < size_; ++i)
        pairs_[i - 1] = old_pairs[i];
    size_--;
}

int JsonObject::index_of(const String& key) const {
    for (int i = 0; i < size_; ++i)
        if (pairs_[i].get_key() == key)
            return i;

    return -1;
}

void JsonObject::remove(const String& key) {
    int index = index_of(key);
    if (index >= 0)
        remove(static_cast<const size_t>(index));
}

bool JsonObject::exists(const String& key) const {
    return index_of(key) >= 0;
}

size_t JsonObject::size() const {
    return size_;
}

bool JsonObject::empty() const {
    return size_ == 0;
}


