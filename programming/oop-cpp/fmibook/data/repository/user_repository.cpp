//
// Created by valio_stoyanov on 5/16/18.
//

#include <fstream>
#include "user_repository.h"

const char UserRepository::kUserPostsFileExtension[] = ".dat";

UserRepository& UserRepository::instance() {
    static UserRepository user_repository;
    return user_repository;
}

bool UserRepository::add_post(const User& user, const Post& post) {
    String filename(user.get_nickname());
    filename.append(String(kUserPostsFileExtension));
    std::ofstream file(filename.c_str(), std::ios::binary | std::ios::app);
    if (file.good()) {
        post.serialize(file);
        return true;
    }

    return false;
}

bool UserRepository::remove_post(const User& user, const unsigned post_id) {

}
