//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "post.h"

Post::Post(const char* content)
: content_(nullptr)
{ set_content(content); }

Post::Post(const Post& other)
: content_(nullptr)
{ set_content(other.content_); }

Post::~Post() {
    delete[] content_;
    content_ = nullptr;
}

Post& Post::operator=(const Post& other) {
    if (this != &other)
        set_content(other.content_);

    return *this;
}

void Post::set_content(const char* content) {
    delete[] content_;
    content_ = nullptr;

    if (content != nullptr) {
        content_ = new char[strlen(content) + 1];
        strcpy(content_, content);
    }
}

const char* Post::get_content() const {
    return content_;
}
