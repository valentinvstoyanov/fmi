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
  static const char kSavedPostsFileExtension[];
    static const char kIdFilename[];
    PostRepository() = default;
public:
    PostRepository(const PostRepository&) = delete;
    PostRepository& operator=(const PostRepository&) = delete;
    static unsigned next_id();
    static PostRepository& instance();
    bool generate_post(const String&, const Post&) const;
    bool save_post(const User&, const Post&) const;

};


#endif //FMIBOOK_POST_REPOSITORY_H
