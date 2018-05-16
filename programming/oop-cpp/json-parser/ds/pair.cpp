//
// Created by valio_stoyanov on 5/14/18.
//

#include "pair.h"
#include "../models/json_null.h"

Pair::Pair(const String& key, const JsonValue& value)
: key_(key), value_(value)
{}

void Pair::set_value(const JsonValue& value) {
    value_ = value;
}

void Pair::set_key(const String& key) {
    key_ = key;
}

void Pair::set_pair(const String& key, const JsonValue& value) {
    value_ = value;
    key_ = key;
}

const String& Pair::get_key() const {
    return key_;
}

const JsonValue& Pair::get_value() const {
    return value_;
}

Pair::Pair()
: key_(nullptr), value_(JsonNull::instance())
{}
