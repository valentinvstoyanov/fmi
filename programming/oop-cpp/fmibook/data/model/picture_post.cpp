//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "picture_post.h"

String PicturePost::toHtml() const {
  String result("<img src=\"");
  result.Append(get_content());
  result.Append(String("\">"));
  return result;
}

PicturePost::PicturePost()
    : Post() {}

PicturePost::PicturePost(const String& path, const unsigned id)
    : Post(path, id) {}

Post::Type PicturePost::get_type() const {
  return kPicture;
}

Post* PicturePost::clone() const {
  return new PicturePost(*this);
}
