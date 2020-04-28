//
// Created by valio_stoyanov on 5/24/18.
//

#ifndef FMIBOOK_STAT_H
#define FMIBOOK_STAT_H

#include <cstddef>
#include "user.h"
#include "../../ds/user_array.h"

class Stat {
  size_t users_count_;
  size_t moderators_count_;
  UserArray users_with_most_posts_;
  UserArray blocked_users_;
  size_t blocked_users_count_;
  UserArray youngest_users_;
  UserArray oldest_users_;
 public:
  Stat(const size_t users_count,
       const size_t moderators_count,
       const UserArray& users_with_most_posts,
       const UserArray& blocked_users,
       const size_t blocked_users_count,
       const UserArray& youngest_users,
       const UserArray& oldest_users);
  Stat();

  size_t GetUsersCount() const;
  void SetUsersCount(const size_t);

  size_t GetModeratorsCount() const;
  void SetModeratorsCount(const size_t);

  const UserArray& GetUsersWithMostPosts() const;
  void SetUsersWithMostPosts(const UserArray&);

  const UserArray& GetBlockedUsers() const;
  void SetBlockedUsers(const UserArray&);

  size_t GetBlockedUsersCount() const;
  void SetBlockedUsersCount(const size_t);

  const UserArray& GetYoungestUsers() const;
  void SetYoungestUsers(const UserArray&);

  const UserArray& GetOldestUsers() const;
  void SetOldestUsers(const UserArray&);
};

#endif //FMIBOOK_STAT_H
