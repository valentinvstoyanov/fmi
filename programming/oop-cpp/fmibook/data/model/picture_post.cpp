//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "picture_post.h"

String PicturePost::toHtml() const {
    String result("<img src=\"");
    result.append(get_content());
    result.append(String("\">"));
    return result;
}

PicturePost::PicturePost()
: Post() {}

PicturePost::PicturePost(const String& path, const unsigned id)
: Post(path, id) {}
