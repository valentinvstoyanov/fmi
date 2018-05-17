//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_POST_REPOSITORY_H
#define FMIBOOK_POST_REPOSITORY_H


#include "../model/user.h"
#include "../model/post.h"
#include "../../ds/mystring.h"

class PostRepository {
    static const char kPostFileExtension[];
    static const char kIdFilename[];
    PostRepository() = default;
public:
    PostRepository(const PostRepository&) = delete;
    PostRepository& operator=(const PostRepository&) = delete;
    static unsigned next_id();
    static PostRepository& instance();
    bool save_post(String&, const Post&) const;
};


#endif //FMIBOOK_POST_REPOSITORY_H
