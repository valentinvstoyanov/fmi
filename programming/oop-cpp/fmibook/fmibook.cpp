//
// Created by valio_stoyanov on 5/5/18.
//

#include <iostream>
#include "fmibook.h"

User& Fmibook::GetUserByNickname(const String& nickname) {
  if (admin_.GetNickname() == nickname)
    return admin_;

  for (size_t i = 0; i < moderators_.Size(); ++i)
    if (moderators_.At(i).GetNickname() == nickname)
      return moderators_.At(i);

  for (size_t i = 0; i < users_.Size(); ++i)
    if (users_.At(i).GetNickname() == nickname)
      return users_.At(i);

  throw NoSuchUserException();
}

UserArray Fmibook::GetUsersWithMostPosts() const {
  UserArray arr;
  arr.PushBack(admin_);
  for (size_t i = 0; i < moderators_.Size(); ++i)
    if (moderators_.At(i).getPostsCount() >= arr.Front().getPostsCount()) {
      if (moderators_.At(i).getPostsCount() > arr.Front().getPostsCount())
        arr.Clear();
      arr.PushBack(moderators_.At(i));
    }
  for (size_t i = 0; i < users_.Size(); ++i)
    if (users_.At(i).getPostsCount() >= arr.Front().getPostsCount()) {
      if (users_.At(i).getPostsCount() > arr.Front().getPostsCount())
        arr.Clear();
      arr.PushBack(users_.At(i));
    }

  return arr;
}

UserArray Fmibook::GetBlockedUsers() const {
  UserArray arr;
  for (size_t i = 0; i < moderators_.Size(); ++i)
    if (moderators_.At(i).IsBlocked())
      arr.PushBack(moderators_.At(i));
  for (size_t i = 0; i < users_.Size(); ++i)
    if (users_.At(i).IsBlocked())
      arr.PushBack(users_.At(i));

  return arr;
}

UserArray Fmibook::GetYoungestUsers() const {
  UserArray arr;
  arr.PushBack(admin_);
  for (size_t i = 0; i < moderators_.Size(); ++i)
    if (moderators_.At(i).GetAge() <= arr.Front().GetAge()) {
      if (moderators_.At(i).GetAge() < arr.Front().GetAge())
        arr.Clear();
      arr.PushBack(moderators_.At(i));
    }
  for (size_t i = 0; i < users_.Size(); ++i)
    if (users_.At(i).GetAge() <= arr.Front().GetAge()) {
      if (users_.At(i).GetAge() < arr.Front().GetAge())
        arr.Clear();
      arr.PushBack(users_.At(i));
    }

  return arr;
}

UserArray Fmibook::GetOldestUsers() const {
  UserArray arr;
  arr.PushBack(admin_);
  for (size_t i = 0; i < moderators_.Size(); ++i)
    if (moderators_.At(i).GetAge() >= arr.Front().GetAge()) {
      if (moderators_.At(i).GetAge() > arr.Front().GetAge())
        arr.Clear();
      arr.PushBack(moderators_.At(i));
    }
  for (size_t i = 0; i < users_.Size(); ++i)
    if (users_.At(i).GetAge() >= arr.Front().GetAge()) {
      if (users_.At(i).GetAge() > arr.Front().GetAge())
        arr.Clear();
      arr.PushBack(users_.At(i));
    }

  return arr;
}

Fmibook::Fmibook(const User& admin)
    : admin_(admin), moderators_(), users_() {}

bool Fmibook::ExistsNickname(const String& nickname) {
  try {
    GetUserByNickname(nickname);
    return true;
  } catch (const NoSuchUserException& e) {
    return false;
  }
}

void Fmibook::AddUser(const String& actor_nickname, const User& user) {
  if (admin_.GetNickname() == actor_nickname) {
    if (ExistsNickname(user.GetNickname()))
      throw NicknameExistsException();

    if (user.IsModerator())
      moderators_.PushBack(user);
    else
      users_.PushBack(user);
  } else {
    throw NoPermissionException();
  }
}

void Fmibook::RemoveUser(const String& actor_nickname, const String& user_nickname) {
  if (admin_.GetNickname() == actor_nickname) {
    for (size_t i = 0; i < users_.Size(); ++i)
      if (users_.At(i).GetNickname() == user_nickname) {
        users_.removeAt(i);
        return;
      }
    for (size_t i = 0; i < moderators_.Size(); ++i)
      if (moderators_.At(i).GetNickname() == user_nickname) {
        moderators_.removeAt(i);
        return;
      }
    throw NoSuchUserException();
  } else {
    throw NoPermissionException();
  }
}

void Fmibook::AddPost(const String& actor_nickname, const Post& post) {
  User user = GetUserByNickname(actor_nickname);
  if (user.IsBlocked())
    throw NoPermissionException("User is blocked so he cannot add new posts.");
  user.AddPost(post);
}

void Fmibook::RemovePost(const String& actor_nickname, const unsigned post_id) {
  User user = GetUserByNickname(actor_nickname);
  if (user.IsAdmin() || user.IsModerator()) {
    if (!user.DeletePost(post_id)) {
      bool is_deleted = false;
      for (size_t i = 0; i < users_.Size() && !is_deleted; ++i)
        is_deleted &= users_.At(i).DeletePost(post_id);
      for (size_t i = 0; i < moderators_.Size() && !is_deleted; ++i)
        is_deleted &= moderators_.At(i).DeletePost(post_id);
      if (!is_deleted)
        throw NoSuchPostException();
    }
  } else {
    if (!user.DeletePost(post_id))
      throw NoSuchPostException();
  }
}

void Fmibook::BlockUnblock(const String& actor_nickname,
                           const String& user_nickname,
                           const bool blocked) {
  User actor = GetUserByNickname(actor_nickname);
  User user;
  if (actor_nickname == user_nickname)
    user = actor;
  else
    user = GetUserByNickname(user_nickname);

  if (actor.IsAdmin() || actor.IsModerator())
    user.SetBlocked(blocked);
  else
    throw NoPermissionException();
}

Stat Fmibook::GetStats() const {
  UserArray users_with_most_posts = GetUsersWithMostPosts();
  UserArray blocked_users = GetBlockedUsers();
  UserArray youngest_users = GetYoungestUsers();
  UserArray oldests_users = GetOldestUsers();
  Stat stat(users_.Size(),
            moderators_.Size(),
            users_with_most_posts,
            blocked_users,
            blocked_users.Size(),
            youngest_users,
            oldests_users);
  return stat;
}
void Fmibook::Persist() const {

}
bool Fmibook::ViewPost(const String& actor_nickname, const unsigned post_id) {
  //TODO: check res in Client
  //TODO: implement it
  return false;
}
bool Fmibook::ViewAllPosts(const String& actor_nickname, const String& user_nickname) {
  //TODO: check res in Client
  //TODO: implement it
  return false;
}

Fmibook::NoSuchUserException::NoSuchUserException(const char* what/* = "No such user."*/)
    : runtime_error(what) {}

Fmibook::NoPermissionException::NoPermissionException(const char* what/* = "No permissions to perform the operation."*/)
    : runtime_error(what) {}

Fmibook::NoSuchPostException::NoSuchPostException(const char* what/* = "No such post."*/)
    : runtime_error(what) {}

Fmibook::NicknameExistsException::NicknameExistsException(const char* what/* = "Nickname already taken."*/)
    : runtime_error(what) {}
