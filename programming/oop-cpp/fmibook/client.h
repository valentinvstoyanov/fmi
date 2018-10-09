//
// Created by valio_stoyanov on 5/24/18.
//

#ifndef FMIBOOK_CLIENT_H
#define FMIBOOK_CLIENT_H

#include "fmibook.h"

class Client {
  static const unsigned kMaxInputLen;
  Fmibook fmibook_;
  static String GetNextWord(const String&, const size_t start, const char delim = ' ');
  void PrintUsers(const UserArray&, const char* prefix = "");
  void PrintStats(const Stat&);
  static User ParseUser(const String&);
  Post* ParsePost(const String&);
  static int ParseInt(const String&);
  void OnAddUser(const String&, const String&, const User::Role = User::Role::kNormal);
  void OnRemoveUser(const String&, const String&);
  void OnBlockUnblockUser(const String&, const String&, const bool);
  void OnAddPost(const String&, const String&);
  void OnRemovePost(const String&, const String&);
  void OnViewPost(const String&, const String&);
  void OnViewAllPosts(const String&, const String&);
 public:
  explicit Client(Fmibook&);
  static String ReadInput();
  static User ReadAdmin();
  bool ProcessInput(const String&);

  class ParseException : public std::runtime_error {
   public:
    explicit ParseException(const char*);
  };
};

#endif //FMIBOOK_CLIENT_H
