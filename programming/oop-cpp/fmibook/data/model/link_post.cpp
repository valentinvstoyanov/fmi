//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "link_post.h"

char* LinkPost::toHtml() const {
    char* result = new char[15 + strlen(get_content()) + strlen(get_description()) + 1];
    strcpy(result, "<a href=\"");
    strcat(result, get_content());
    strcat(result, "\">");
    strcat(result, description_);
    strcat(result, "</a>");

    return result;
}

LinkPost::LinkPost()
: Post(nullptr), description_(nullptr)
{}

LinkPost::LinkPost(const char* content, const char* description)
: Post(content), description_(nullptr)
{ set_description(description); }

LinkPost::LinkPost(const LinkPost& other)
: Post(other), description_(nullptr)
{ set_description(other.description_); }

LinkPost::~LinkPost() {
    delete[] description_;
    description_ = nullptr;
}

LinkPost& LinkPost::operator=(const LinkPost& other) {
    if (this != &other) {
        Post::operator=(other);
        set_description(other.description_);
    }

    return *this;
}

void LinkPost::set_description(const char* description) {
    delete[] description_;
    description_ = nullptr;

    if (description != nullptr) {
        description_ = new char[strlen(description) + 1];
        strcpy(description_, description);
    }
}

const char* LinkPost::get_description() const {
    return description_;
}
