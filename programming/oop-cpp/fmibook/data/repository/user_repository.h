//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_USER_REPOSITORY_H
#define FMIBOOK_USER_REPOSITORY_H


#include "../model/user.h"
#include "../model/post.h"

class UserRepository {
    UserRepository() = default;
    static const char kUserPostsFileExtension[];
    static const char kUserNicknamesFileName[];
public:
    UserRepository(const UserRepository&) = delete;
    UserRepository& operator=(const UserRepository&) = delete;
    static UserRepository& instance();
    bool exists(const User&);
};


#endif //FMIBOOK_USER_REPOSITORY_H
