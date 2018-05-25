//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "user.h"

User::User()
    : nickname_(nullptr), age_(0), blocked_(false), role_(kNormal) {}

User::User(const String& nickname, const unsigned age, const Role role)
    : nickname_(nickname), age_(age), blocked_(false), role_(role) {}

User::User(const User& user)
    : nickname_(user.nickname_),
      age_(user.age_),
      blocked_(user.blocked_),
      role_(user.role_) {}

User::~User() {}

User& User::operator=(const User& other) {
  if (this != &other) {
    age_ = other.age_;
    nickname_ = other.nickname_;
    blocked_ = other.blocked_;
    role_ = other.role_;
  }

  return *this;
}

void User::SetNickname(const String& nickname) {
  nickname_ = nickname;
}

void User::SetAge(const unsigned age) {
  age_ = age;
}

unsigned User::GetAge() const {
  return age_;
}

const String& User::GetNickname() const {
  return nickname_;
}

void User::SetBlocked(const bool blocked) {
  blocked_ = blocked;
}

void User::SetRole(const User::Role role) {
  role_ = role;
}

bool User::IsAdmin() const {
  return role_ == kAdmin;
}

bool User::IsModerator() const {
  return role_ == kModerator;
}

bool User::IsBlocked() const {
  return blocked_;
}

void User::AddPost(const Post& post) {
  posts_.PushBack(post);
}

bool User::DeletePost(const unsigned id) {
  for (size_t i = 0; i < posts_.Size(); ++i)
    if (posts_.At(i).get_id() == id) {
      posts_.removeAt(i);
      return true;
    }

  return false;
}
size_t User::getPostsCount() const {
  return posts_.Size();
}

std::ostream& operator<<(std::ostream& out, const User& user) {
  out << '(' << user.GetNickname() << ", " << user.GetAge() << ')';
  return out;
}



