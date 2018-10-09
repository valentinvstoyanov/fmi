//
// Created by valio_stoyanov on 5/16/18.
//

#include <fstream>
#include <iostream>
#include "user_repository.h"

const char UserRepository::kHasRecordsFilename[] = "records.dat";
const char UserRepository::kAdminFilename[] = "admin.dat";
const char UserRepository::kUsersFilename[] = "users.dat";
const char UserRepository::kModeratorsFilename[] = "moderators.dat";

UserArray UserRepository::LoadUsers(const char* filename) const {
  std::ifstream file(filename, std::ios::binary | std::ios::in);
  if (!file.good()) throw LoadException("Failed to retrieve users");

  size_t users_count;
  file.read(reinterpret_cast<char*>(&users_count), sizeof(size_t));
  UserArray res(users_count);

  User user;
  for (size_t i = 0; i < users_count; ++i) {
    user.Deserialize(file);
    res.PushBack(user);
  }

  return res;
}

bool UserRepository::SaveUsers(const char* filename,
                               const UserArray& users) const {
  std::ofstream file(filename, std::ios::binary | std::ios::out | std::ios::trunc);
  if (!file.good()) return false;

  const size_t kUsersCount = users.Size();
  file.write(reinterpret_cast<const char*>(&kUsersCount), sizeof(size_t));

  for (size_t i = 0; i < kUsersCount; ++i) {
    const User& kUser = users.At(i);
    kUser.Serialize(file);
  }

  return true;
}

UserRepository& UserRepository::Instance() {
  static UserRepository user_repository;
  return user_repository;
}

void UserRepository::SetHasRecords() const {
  std::ofstream file(kHasRecordsFilename,
                     std::ios::binary | std::ios::out | std::ios::trunc);
  bool b = true;
  file.write(reinterpret_cast<char*>(&b), sizeof(bool));
}

bool UserRepository::HasRecords() const {
  static bool file_checked = false;
  static bool has_records = false;
  if (!file_checked) {
    file_checked = true;
    std::ifstream file(kHasRecordsFilename, std::ios::binary | std::ios::in);
    if (file.good()) {
      file.read(reinterpret_cast<char*>(&has_records), sizeof(bool));
    }
  }

  return has_records;
}

User UserRepository::LoadAdmin() const {
  if (!HasRecords()) throw NoRecordsException();

  std::ifstream file(kAdminFilename, std::ios::binary | std::ios::in);
  if (!file.good()) throw LoadException("Failed to retrieve admin.");
  User user;
  user.Deserialize(file);

  return user;
}

UserArray UserRepository::LoadModerators() const {
  return LoadUsers(kModeratorsFilename);
}

UserArray UserRepository::LoadUsers() const {
  return LoadUsers(kUsersFilename);
}

bool UserRepository::SaveAdmin(const User& admin) const {
  std::ofstream file(kAdminFilename,
                     std::ios::binary | std::ios::out | std::ios::trunc);
  if (!file.good()) return false;
  admin.Serialize(file);
  return true;
}

bool UserRepository::SaveModerators(const UserArray& moderators) const {
  return SaveUsers(kModeratorsFilename, moderators);
}

bool UserRepository::SaveUsers(const UserArray& users) const {
  return SaveUsers(kUsersFilename, users);
}

UserRepository::NoRecordsException::NoRecordsException(const char* what/* = "No records."*/)
    : runtime_error(what) {}

UserRepository::LoadException::LoadException(const char* what/* = "Failed to load records."*/)
    : runtime_error(what) {}
