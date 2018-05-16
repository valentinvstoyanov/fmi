//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_string.h"

JsonString::JsonString(const char* str)
: string_(str)
{}

JsonString::JsonString(const String& str)
: string_(str) {}

JsonString::JsonString(const JsonString& other) = default;

JsonString::~JsonString() = default;

JsonString& JsonString::operator=(const JsonString& other) {
    if (this != &other)
        string_ = other.string_;

    return *this;
}


