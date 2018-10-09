//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_POST_REPOSITORY_H
#define FMIBOOK_POST_REPOSITORY_H

#include "../model/user.h"
#include "../model/post.h"
#include "../../ds/mystring.h"

class PostRepository {
  static const char kGeneratePostFileExtension[];
  static const char kIdFilename[];
  PostRepository() = default;
 public:
  PostRepository(const PostRepository&) = delete;
  PostRepository& operator=(const PostRepository&) = delete;

  static unsigned NextId();
  static PostRepository& Instance();

  bool GeneratePost(const String&, const Post&) const;
  bool GeneratePost(const String&, const PostArray&) const;

  PostArray LoadPosts(std::istream&);
  bool SavePosts(std::ostream&, const PostArray&);

  class LoadPostsException : public std::runtime_error {
   public:
    explicit LoadPostsException(const char* what = "Failed to load posts.");
  };
};

#endif //FMIBOOK_POST_REPOSITORY_H
