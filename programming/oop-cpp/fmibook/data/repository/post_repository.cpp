//
// Created by valio_stoyanov on 5/16/18.
//

#include <fstream>
#include <iostream>
#include "post_repository.h"

const char PostRepository::kGeneratePostFileExtension[] = ".html";
const char PostRepository::kSavedPostsFileExtension[] = ".dat";
const char PostRepository::kIdFilename[] = "POST_ID.dat";

unsigned PostRepository::next_id() {
  static unsigned id = 0;
  static bool file_checked = false;

  if (!file_checked) {
    std::ifstream file(kIdFilename, std::ios::binary);
    if (!file.good())
      return id++;
    file.read(reinterpret_cast<char*>(&id), sizeof(unsigned));
    file_checked = true;
  }

  return id++;
}

PostRepository& PostRepository::instance() {
  static PostRepository post_repository;
  return post_repository;
}

bool PostRepository::generate_post(const String& name, const Post& post) const {
  String filename(name);
  filename.Append(String(kGeneratePostFileExtension));
  String content("<!DOCTYPE html>\n<html>\n\n\t<body>\n\t\t");
  content.Append(post.toHtml());
  content.Append(String("\n\t</body>\n\n</html>\n"));

  std::ofstream file(filename.CStr(), std::ios::out | std::ios::trunc);
  if (file.good()) {
    file << content.CStr();
    file.close();
    return true;
  }

  return false;
}

bool PostRepository::generate_post(const String& name, const PostArray& posts) const {
  String filename(name);
  filename.Append(String(kGeneratePostFileExtension));
  String content("<!DOCTYPE html>\n<html>\n\n\t<body>\n\t\t");
  for (size_t i = 0; i < posts.Size() ; ++i)
    content.Append(posts.At(i).toHtml());
  content.Append(String("\n\t</body>\n\n</html>\n"));

  std::ofstream file(filename.CStr(), std::ios::out | std::ios::trunc);
  if (file.good()) {
    file << content.CStr();
    file.close();
    return true;
  }

  return false;
}

bool PostRepository::save_post(const User& user, const Post& post) const {
  String filename(user.GetNickname());
  filename.Append(String(kSavedPostsFileExtension));
  std::ofstream file(filename.CStr(), std::ios::out | std::ios::app);
  if (file.good()) {
    post.serialize(file);
    return true;
  }

  return false;
}

