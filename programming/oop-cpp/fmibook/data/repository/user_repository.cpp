//
// Created by valio_stoyanov on 5/16/18.
//

#include <fstream>
#include "user_repository.h"

const char UserRepository::kUserPostsFileExtension[] = ".dat";
const char UserRepository::kUserNicknamesFileName[] = "$$nicknames.dat";

UserRepository& UserRepository::instance() {
    static UserRepository user_repository;
    return user_repository;
}

bool UserRepository::exists(const User& user) {
    std::ifstream file(kUserNicknamesFileName, std::ios::binary);
    if (file.good()) {
        size_t nicknames_size = 0;
        file.read(reinterpret_cast<char*>(&nicknames_size), sizeof(size_t));
        if (nicknames_size > 0) {
            String nickname;
            for (size_t i = 0; file && i < nicknames_size; ++i) {
              nickname.Deserialize(file);
                if (nickname == user.GetNickname())
                    return true;
            }
        }
    }

    return false;
}
