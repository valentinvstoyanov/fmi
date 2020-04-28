//
// Created by valio_stoyanov on 5/16/18.
//

#include <fstream>
#include <iostream>
#include "post_repository.h"
#include "../model/text_post.h"
#include "../model/picture_post.h"
#include "../model/link_post.h"

const char PostRepository::kGeneratePostFileExtension[] = ".html";
const char PostRepository::kIdFilename[] = "post_id.dat";

unsigned PostRepository::NextId() {
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

PostRepository& PostRepository::Instance() {
  static PostRepository post_repository;
  return post_repository;
}

bool PostRepository::GeneratePost(const String& name, const Post& post) const {
  String filename(name);
  filename.Append(String(kGeneratePostFileExtension));
  String content("<!DOCTYPE html>\n<html>\n\n\t<body>\n\t\t");
  content.Append(post.ToHtml());
  content.Append(String("\n\t</body>\n\n</html>\n"));

  std::ofstream file(filename.CStr(), std::ios::out | std::ios::trunc);
  if (file.good()) {
    file << content.CStr();
    file.close();
    return true;
  }

  return false;
}

bool PostRepository::GeneratePost(const String& name, const PostArray& posts) const {
  String filename(name);
  filename.Append(String(kGeneratePostFileExtension));
  String content("<!DOCTYPE html>\n<html>\n\n\t<body>\n\t\t");
  for (size_t i = 0; i < posts.Size() ; ++i) {
    const Post& post = posts.At(i);
    content.Append(post.ToHtml());
    content.PushBack('\n');
  }
  content.Append(String("\n\t</body>\n\n</html>\n"));

  std::ofstream file(filename.CStr(), std::ios::out | std::ios::trunc);
  if (file.good()) {
    file << content.CStr();
    file.close();
    return true;
  }

  return false;
}

PostArray PostRepository::LoadPosts(std::istream& in) {
  if (!in.good()) throw LoadPostsException();

  size_t posts_count;
  in.read(reinterpret_cast<char*>(&posts_count), sizeof(size_t));

  PostArray res(posts_count);

  for (size_t i = 0; i < posts_count; ++i) {
    Post::Type post_type;
    in.read(reinterpret_cast<char*>(&post_type), sizeof(Post::Type));
    switch (post_type) {
      case Post::Type::kText: {
        TextPost text_post;
        text_post.Deserialize(in);
        res.PushBack(text_post);
        break;
        }
      case Post::Type::kPicture: {
        PicturePost picture_post;
        picture_post.Deserialize(in);
        res.PushBack(picture_post);
        break;}
      case Post::Type::kLink: {
        LinkPost linkPost;
        linkPost.Deserialize(in);
        res.PushBack(linkPost);
        break;}
      default:
        throw LoadPostsException("Cannot deal with this type of post.");
    }
  }

  return res;
}

bool PostRepository::SavePosts(std::ostream& out, const PostArray& posts) {
  if (!out.good()) return false;

  size_t posts_count = posts.Size();
  out.write(reinterpret_cast<const char*>(&posts_count), sizeof(size_t));

  for (size_t i = 0; i < posts_count; ++i) {
    const Post& post = posts.At(i);
    const Post::Type type = post.GetType();
    out.write(reinterpret_cast<const char*>(&type), sizeof(Post::Type));
    post.Serialize(out);
  }

  return true;
}

PostRepository::LoadPostsException::LoadPostsException(const char* what/* = "Failed to load posts."*/)
    : runtime_error(what) {}
