//
// Created by valio_stoyanov on 5/24/18.
//

#include <iostream>
#include "client.h"
#include "data/model/text_post.h"
#include "data/model/link_post.h"
#include "data/model/picture_post.h"

String Client::GetNextWord(const String& str, const size_t start, const char delim/* = ' '*/) {
  String res;
  for (size_t i = start; i < str.Length() && str.At(i) != delim; ++i)
    res.PushBack(str.At(i));

  return res;
}

void Client::PrintUsers(const UserArray& users, const char* prefix/* = ""*/) {
  for (size_t i = 0; i < users.Size(); ++i)
    std::cout << prefix << users.At(i) << '\n';
}

void Client::PrintStats(const Stat& stat) {
  std::cout << "Fmibook Statistics:\n"
              << "\t- Users count: " << stat.GetUsersCount() << '\n'
              << "\t- Moderators count: " << stat.GetModeratorsCount()
              << std::endl;

  const UserArray& users_with_most_posts = stat.GetUsersWithMostPosts();
  if (!users_with_most_posts.Empty()) {
    std::cout << "\t- "
              << (users_with_most_posts.Size() > 1 ? "Users" : "User")
              << " with most posts:\n";
    PrintUsers(users_with_most_posts, "\t\t- ");
  }

  std::cout << "\t- Blocked users count: " << stat.GetBlockedUsersCount() << std::endl;

  const UserArray& blocked_users = stat.GetBlockedUsers();
  if (!blocked_users.Empty()) {
    std::cout << "\t- Blocked "
              << (stat.GetBlockedUsersCount() > 1 ? "users: " : "user: ")
              << '\n';
    PrintUsers(blocked_users, "\t\t- ");
  }

  const UserArray& youngest_users = stat.GetYoungestUsers();
  if (!youngest_users.Empty()) {
    std::cout << "\t- Youngest "
              << (youngest_users.Size() > 1 ? "users: " : "user: ")
              << '\n';
    PrintUsers(youngest_users, "\t\t- ");
  }

  const UserArray& oldest_users = stat.GetOldestUsers();
  if (!oldest_users.Empty()) {
    std::cout << "\t- Oldest "
              << (oldest_users.Size() > 1 ? "users: " : "user: ")
              << '\n';
    PrintUsers(oldest_users, "\t\t- ");
  }
}


User Client::ParseUser(const String& str) {
  User res;
  String nickname = GetNextWord(str, 0);
  String age_str = GetNextWord(str, nickname.Length() + 1, '\0');
  res.SetNickname(nickname);
  res.SetAge(static_cast<const unsigned>(ParseInt(age_str)));
  return res;
}

Post* Client::ParsePost(const String& str) {
  String post_type = GetNextWord(str, 0);
  if (post_type == String("[text]")) {
    String txt = GetNextWord(str, post_type.Length() + 1, '\0');
    return new TextPost(txt);
  } else if (post_type == String("[url]")) {
    String url = GetNextWord(str, post_type.Length() + 1);
    String descr = GetNextWord(str, post_type.Length() + url.Length() + 2, '\0');
    return new LinkPost(url, descr);
  } else if (post_type == String("[image]")) {
    String img = GetNextWord(str, post_type.Length() + 1, '\0');
    return new PicturePost(img);
  } else {
    throw ParseException("Failed to parse post from the input.");
  }
}

int Client::ParseInt(const String& str) {
  if (str.Empty())
    throw ParseException("Failed to parse integer from the input.");

  for (size_t i = 0; i < str.Length(); ++i)
    if (str.At(i) < '0' || str.At(i) > '9')
      throw ParseException("Failed to parse integer from the input.");

  return atoi(str.CStr());
}

void Client::OnAddUser(const String& actor,
                       const String& subject,
                       const User::Role role/* = User::Role::kNormal*/) {
  User user;
  try {
    user = ParseUser(subject);
  } catch (const ParseException&) {
    std::cout << "Failed to parse the user data."
                 " Please check your input and try again!" << std::endl;
    return;
  }
  user.SetRole(role);

  try {
    fmibook_.AddUser(actor, user);
  } catch (const Fmibook::NicknameExistsException& e) {
    std::cout << e.what()
              << " Please choose another nickname and try again!"
              << std::endl;
    return;
  } catch (const Fmibook::NoPermissionException&) {
    std::cout << actor
              << " doesn't have permission to add new users."
              << std::endl;
    return;
  }

  std::cout << "New "
            << (role == User::Role::kModerator ? "moderator" : "user")
            << " added: "
            << user
            << '.'
            << std:: endl;
}

void Client::OnRemoveUser(const String& actor, const String& subject) {
  try {
    fmibook_.RemoveUser(actor, subject);
  } catch (const Fmibook::NoSuchUserException&) {
    std::cout << "User with nickname "
              << '"'
              << subject
              << '"'
              << " doesn't exist."
              << std::endl;
    return;
  } catch (const Fmibook::NoPermissionException&) {
    std::cout << actor
              << " doesn't have permission to remove users."
              << std::endl;
    return;
  }

  std::cout << subject << " deleted by " << actor << std::endl;
}

void Client::OnBlockUnblockUser(const String& actor,
                                const String& subject,
                                const bool block) {
  try {
    fmibook_.BlockUnblock(actor, subject, block);
  } catch (const Fmibook::NoPermissionException& e) {
    std::cout << actor
              << " doesn't have permission to "
              << (block ? "block" : "unblock")
              << " users."
              << std::endl;
    return;
  }

  std::cout << subject
            << (block ? " blocked" : " unblocked")
            << " by "
            << actor
            << std::endl;
}

void Client::OnAddPost(const String& actor, const String& subject) {
  Post* post = nullptr;

  try {
    post = ParsePost(subject);
  } catch (const ParseException& e) {
    std::cout << e.what() << " Please check it and try again" << std::endl;
    return;
  }

  try {
    fmibook_.AddPost(actor, *post);
    std::cout << "Post " << post->get_id() << " added." << std::endl;
  } catch (const Fmibook::NoPermissionException& e) {
    std::cout << e.what() << std::endl;
  }

  delete post;
}

void Client::OnViewPost(const String& actor, const String& subject) {
  try {
    const unsigned post_id = static_cast<const unsigned>(ParseInt(subject));
    if (fmibook_.ViewPost(actor, post_id))
      std::cout << "HTML view for post " << post_id << " generated." << std::endl;
  } catch (const Fmibook::NoSuchPostException& e) {
    std::cout << e.what() << std::endl;
  } catch (const ParseException& e) {
    std::cout << "Failed to parse the input post id." << std::endl;
  } catch (const Fmibook::NoPermissionException& e) {
    std::cout << e.what() << std::endl;
  }
}

void Client::OnViewAllPosts(const String& actor, const String& subject) {
  try {
    if (fmibook_.ViewAllPosts(actor, subject))
      std::cout << "HTML view for all " << subject << "'s posts created." << std::endl;
  } catch (const Fmibook::NoSuchUserException& e) {
    std::cout << "Failed to find user with nickname: " << subject << std::endl;
  } catch (const Fmibook::NoPermissionException& e) {
    std::cout << e.what() << std::endl;
  }
}

bool Client::ProcessInput(const String& input) {
  if (input == String("quit")) {
    fmibook_.Persist();
    return false;
  } else if (input == String("stats")) {
    Stat stat = fmibook_.GetStats();
    PrintStats(stat);
  } else {
    String actor = GetNextWord(input, 0);
    String action = GetNextWord(input, actor.Length() + 1);
    String subject = GetNextWord(input, actor.Length() + action.Length() + 2, '\0');

    if (action == String("add_moderator"))
      OnAddUser(actor, subject, User::Role::kModerator);
    else if (action == String("add_user"))
      OnAddUser(actor, subject);
    else if (action == String("remove_user"))
      OnRemoveUser(actor, subject);
    else if (action == String("block"))
      OnBlockUnblockUser(actor, subject, true);
    else if (action == String("unblock"))
      OnBlockUnblockUser(actor, subject, false);
    else if (action == String("post"))
      OnAddPost(actor, subject);
    else if (action == String("view_post"))
      OnViewPost(actor, subject);
    else if (action == String("view_all_posts"))
      OnViewAllPosts(actor, subject);
    else
      std::cout << "No such command. Please try again with another one!" << std::endl;
  }

  return true;
}
Client::Client(Fmibook& fmibook)
    : fmibook_(fmibook) {}

Client::ParseException::ParseException(const char* what)
    : runtime_error(what) {}