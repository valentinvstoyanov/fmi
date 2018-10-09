//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_USER_H
#define FMIBOOK_USER_H

#include "../../ds/mystring.h"
#include "../../ds/post_array.h"

class User {
  String nickname_;
  unsigned age_;
  bool blocked_;
  PostArray posts_;
 public:
  enum Role { kAdmin, kModerator, kNormal };

  User();
  User(const String&, const unsigned, const Role);
  User(const User&);
  ~User();
  User& operator=(const User&);
  friend std::ostream& operator<<(std::ostream&, const User&);

  void SetNickname(const String&);
  void SetAge(const unsigned);
  void SetBlocked(const bool);
  void SetRole(const Role);

  unsigned GetAge() const;
  const String& GetNickname() const;
  const PostArray& GetPosts() const;
  size_t GetPostsCount() const;
  const Post& FindPostById(const unsigned) const;

  bool IsAdmin() const;
  bool IsModerator() const;
  bool IsBlocked() const;

  void AddPost(const Post&);
  bool DeletePost(const unsigned id);

  void Serialize(std::ostream&) const;
  void Deserialize(std::istream&);

  class NoSuchPostException : public std::runtime_error {
   public:
    explicit NoSuchPostException(const char* = "No such post.");
  };

 private:
  Role role_;
};

#endif //FMIBOOK_USER_H
