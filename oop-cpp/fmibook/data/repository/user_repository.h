//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_USER_REPOSITORY_H
#define FMIBOOK_USER_REPOSITORY_H

#include "../model/user.h"
#include "../model/post.h"
#include "../../ds/user_array.h"

class UserRepository {
  static const char kHasRecordsFilename[];
  static const char kAdminFilename[];
  static const char kUsersFilename[];
  static const char kModeratorsFilename[];
  UserArray LoadUsers(const char* filename) const;
  bool SaveUsers(const char* filename, const UserArray& users) const;
  UserRepository() = default;
 public:
  UserRepository(const UserRepository&) = delete;
  UserRepository& operator=(const UserRepository&) = delete;

  static UserRepository& Instance();

  void SetHasRecords() const;
  bool HasRecords() const;

  User LoadAdmin() const;
  UserArray LoadModerators() const;
  UserArray LoadUsers() const;

  bool SaveAdmin(const User&) const;
  bool SaveModerators(const UserArray&) const;
  bool SaveUsers(const UserArray&) const;

 class NoRecordsException : public std::runtime_error {
  public:
   explicit NoRecordsException(const char* what = "No records.");
 };

 class LoadException : public std::runtime_error {
  public:
   explicit LoadException(const char* what = "Failed to load records.");
 };
};

#endif //FMIBOOK_USER_REPOSITORY_H
