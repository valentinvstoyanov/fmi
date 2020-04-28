//
// Created by valio_stoyanov on 5/24/18.
//

#include "stat.h"

Stat::Stat(size_t users_count,
           size_t moderators_count,
           const UserArray& users_with_most_posts,
           const UserArray& blocked_users,
           const size_t blocked_users_count,
           const UserArray& youngest_users,
           const UserArray& oldest_users)
    : users_count_(users_count),
      moderators_count_(moderators_count),
      users_with_most_posts_(users_with_most_posts),
      blocked_users_(blocked_users),
      blocked_users_count_(blocked_users_count),
      youngest_users_(youngest_users),
      oldest_users_(oldest_users) {}

Stat::Stat() = default;

size_t Stat::GetUsersCount() const {
  return users_count_;
}

void Stat::SetUsersCount(const size_t users_count) {
  users_count_ = users_count;
}

size_t Stat::GetModeratorsCount() const {
  return moderators_count_;
}

void Stat::SetModeratorsCount(const size_t moderators_count) {
  moderators_count_ = moderators_count;
}

const UserArray& Stat::GetUsersWithMostPosts() const {
  return users_with_most_posts_;
}

void Stat::SetUsersWithMostPosts(const UserArray& users_with_most_posts) {
  users_with_most_posts_ = users_with_most_posts;
}

const UserArray& Stat::GetBlockedUsers() const {
  return blocked_users_;
}

void Stat::SetBlockedUsers(const UserArray& blocked_users) {
  blocked_users_ = blocked_users;
}

const UserArray& Stat::GetYoungestUsers() const {
  return youngest_users_;
}

void Stat::SetYoungestUsers(const UserArray& youngest_users) {
  youngest_users_ = youngest_users;
}

const UserArray& Stat::GetOldestUsers() const {
  return oldest_users_;
}

void Stat::SetOldestUsers(const UserArray& oldest_users) {
  oldest_users_ = oldest_users;
}

size_t Stat::GetBlockedUsersCount() const {
  return blocked_users_count_;
}
void Stat::SetBlockedUsersCount(const size_t blocked_users_count) {
  blocked_users_count_ = blocked_users_count;
}
