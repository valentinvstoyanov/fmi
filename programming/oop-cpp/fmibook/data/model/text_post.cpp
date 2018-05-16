//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "text_post.h"

char* TextPost::toHtml() const {
    char* result = new char[strlen(get_content()) + 1];
    strcpy(result, get_content());

    return result;
}

TextPost::TextPost(const char* text)
: Post(text)
{}


