//
// Created by valio_stoyanov on 5/5/18.
//

#ifndef FMIBOOK_FMIBOOK_H
#define FMIBOOK_FMIBOOK_H

#include "data/model/user.h"
#include "ds/user_array.h"
#include "data/model/stat.h"

class Fmibook {
  User admin_;
  UserArray moderators_;
  UserArray users_;
  User& GetUserByNickname(const String&);
  const Post& GetPostById(const unsigned) const ;
  UserArray GetUsersWithMostPosts() const;
  UserArray GetBlockedUsers() const;
  UserArray GetYoungestUsers() const;
  UserArray GetOldestUsers() const;
  explicit Fmibook(const User&);
 public:
  bool ExistsNickname(const String&);

  void AddUser(const String& actor_nickname, const User&);
  void RemoveUser(const String& actor_nickname, const String& user_nickname);
  void AddPost(const String& actor_nickname, Post&);
  void RemovePost(const String& actor_nickname, const unsigned post_id);
  void BlockUnblock(const String& actor_nickname, const String& user_nickname, const bool);
  bool ViewPost(const String& actor_nickname, const unsigned post_id);
  bool ViewAllPosts(const String& actor_nickname, const String& user_nickname);
  bool Persist() const;
  Stat GetStats() const;

  static Fmibook From(const User&, const UserArray&, const UserArray&);
  static Fmibook From(const User&);

  class NoSuchUserException : public std::runtime_error {
   public:
    explicit NoSuchUserException(const char* = "No such user.");
  };

  class NoPermissionException : public std::runtime_error {
   public:
    explicit NoPermissionException(const char* = "No permissions to perform the operation.");
  };

  class NoSuchPostException : public std::runtime_error {
   public:
    explicit NoSuchPostException(const char* = "No such post.");
  };

 class NicknameExistsException : public std::runtime_error {
  public:
   explicit NicknameExistsException(const char* = "Nickname already taken.");
 };
};

#endif //FMIBOOK_FMIBOOK_H
