//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "picture_post.h"

PicturePost::PicturePost(const char* path)
: Post(path)
{}

char* PicturePost::toHtml() const {
    char* result = new char[12 + strlen(get_content()) + 1];
    strcpy(result, "<img src=\"");
    strcat(result, get_content());
    strcat(result, "\">");

    return result;
}
