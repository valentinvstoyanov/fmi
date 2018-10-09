//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "user.h"
#include "../repository/post_repository.h"

User::User()
    : nickname_(""), age_(0), blocked_(false), role_(kNormal), posts_() {}

User::User(const String& nickname, const unsigned age, const Role role)
    : nickname_(nickname), age_(age), blocked_(false), role_(role), posts_() {}

User::User(const User& user)
    : nickname_(user.nickname_),
      age_(user.age_),
      blocked_(user.blocked_),
      role_(user.role_),
      posts_(user.posts_) {}

User::~User() {}

User& User::operator=(const User& other) {
  if (this != &other) {
    age_ = other.age_;
    nickname_ = other.nickname_;
    blocked_ = other.blocked_;
    role_ = other.role_;
    posts_ = other.posts_;
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
    if (posts_.At(i).GetId() == id) {
      posts_.removeAt(i);
      return true;
    }

  return false;
}

const PostArray& User::GetPosts() const {
  return posts_;
}

size_t User::GetPostsCount() const {
  return posts_.Size();
}

std::ostream& operator<<(std::ostream& out, const User& user) {
  out << '(' << user.GetNickname() << ", " << user.GetAge() << ')';
  return out;
}

const Post& User::FindPostById(const unsigned post_id) const {
  for (size_t i = 0; i < posts_.Size(); ++i)
    if (posts_.At(i).GetId() == post_id)
      return posts_.At(i);

  throw NoSuchPostException("Cannot find post with that id.");
}

void User::Serialize(std::ostream& out) const {
  nickname_.Serialize(out);
  out.write(reinterpret_cast<const char*>(&age_), sizeof(age_));
  out.write(reinterpret_cast<const char*>(&role_), sizeof(role_));
  out.write(reinterpret_cast<const char*>(&blocked_), sizeof(blocked_));
  PostRepository::Instance().SavePosts(out, posts_);
}

void User::Deserialize(std::istream& in) {
  nickname_.Deserialize(in);
  in.read(reinterpret_cast<char*>(&age_), sizeof(age_));
  in.read(reinterpret_cast<char*>(&role_), sizeof(role_));
  in.read(reinterpret_cast<char*>(&blocked_), sizeof(blocked_));
  posts_ = PostRepository::Instance().LoadPosts(in);
}

User::NoSuchPostException::NoSuchPostException(const char* what/* = "No such post."*/)
    : runtime_error(what) {}



