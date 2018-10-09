//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "picture_post.h"

String PicturePost::ToHtml() const {
  String result("<img src=\"");
  result.Append(GetContent());
  result.Append(String("\">"));
  return result;
}

PicturePost::PicturePost()
    : Post() {}

PicturePost::PicturePost(const String& path, const unsigned id)
    : Post(path, id) {}

Post* PicturePost::Clone() const {
  return new PicturePost(*this);
}

Post::Type PicturePost::GetType() const {
  return kPicture;
}
