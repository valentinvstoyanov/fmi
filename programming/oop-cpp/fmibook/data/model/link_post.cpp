//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "link_post.h"

String LinkPost::ToHtml() const {
  String result("<a href=\"");
  result.Append(GetContent());
  result.Append(String("\">"));
  result.Append(description_);
  result.Append(String("</a>"));
  return result;
}

LinkPost::LinkPost()
    : Post(), description_() {}

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

void LinkPost::SetDescription(const String& description) {
  description_ = description;
}

const String& LinkPost::GetDescription() const {
  return description_;
}

LinkPost::~LinkPost() {}

Post* LinkPost::Clone() const {
  return new LinkPost(*this);
}

void LinkPost::Serialize(std::ostream& ostream) const {
  Post::Serialize(ostream);
  description_.Serialize(ostream);
}

void LinkPost::Deserialize(std::istream& istream) {
  Post::Deserialize(istream);
  description_.Deserialize(istream);
}

Post::Type LinkPost::GetType() const {
  return kLink;
}
