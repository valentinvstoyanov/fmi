//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "link_post.h"

String LinkPost::toHtml() const {
  String result("<a href=\"");
  result.Append(get_content());
  result.Append(String("\">"));
  result.Append(description_);
  result.Append(String("</a>"));
  return result;
}

LinkPost::LinkPost()
    : Post(), description_(nullptr) {}

LinkPost::LinkPost(const String& content, const String& description, const unsigned id)
    : Post(content, id), description_(description) {}

LinkPost::LinkPost(const LinkPost& other)
    : Post(other), description_(other.description_) {}

LinkPost& LinkPost::operator=(const LinkPost& other) {
  if (this != &other) {
    Post::operator=(other);
    description_ = other.description_;
  }

  return *this;
}

void LinkPost::set_description(const String& description) {
  description_ = description;
}

const String& LinkPost::get_description() const {
  return description_;
}

LinkPost::~LinkPost() {}

Post::Type LinkPost::get_type() const {
  return kLink;
}

Post* LinkPost::clone() const {
  return new LinkPost(*this);
}
