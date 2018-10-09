//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "text_post.h"

String TextPost::ToHtml() const {
  String result(GetContent());
  return result;
}

TextPost::TextPost()
    : Post() {}

TextPost::TextPost(const String& text, const unsigned id)
    : Post(text, id) {}

Post* TextPost::Clone() const {
  return new TextPost(*this);
}

Post::Type TextPost::GetType() const {
  return kText;
}

