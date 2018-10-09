//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "post.h"

Post::Post()
    : content_(), id_(0) {}

Post::Post(const String& content, const unsigned id)
    : id_(id), content_(content) {}

Post& Post::operator=(const Post& other) {
  if (this != &other) {
    id_ = other.id_;
    content_ = other.content_;
  }

  return *this;
}

void Post::SetContent(const String& content) {
  content_ = content;
}

const String& Post::GetContent() const {
  return content_;
}

void Post::SetId(const unsigned id) {
  id_ = id;
}

unsigned Post::GetId() const {
  return id_;
}

void Post::Serialize(std::ostream& out) const {
  out.write(reinterpret_cast<const char*>(&id_), sizeof(id_));
  content_.Serialize(out);
}

void Post::Deserialize(std::istream& in) {
  in.read(reinterpret_cast<char*>(&id_), sizeof(id_));
  content_.Deserialize(in);
}
