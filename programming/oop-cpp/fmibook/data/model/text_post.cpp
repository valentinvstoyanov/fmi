//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "text_post.h"

String TextPost::toHtml() const {
  String result(get_content());
  return result;
}

TextPost::TextPost()
    : Post() {}

TextPost::TextPost(const String& text, const unsigned id)
    : Post(text, id) {}

Post::Type TextPost::get_type() const {
  return kText;
}

Post* TextPost::clone() const {
  return new TextPost(*this);
}

