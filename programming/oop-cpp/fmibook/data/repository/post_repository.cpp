//
// Created by valio_stoyanov on 5/16/18.
//

#include <fstream>
#include "post_repository.h"

const char PostRepository::kPostFileExtension[] = ".html";
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

bool PostRepository::save_post(String& name, const Post& post) const {
    name.append(String(kPostFileExtension));
    String content("<!DOCTYPE html>\n<html>\n\n\t<body>\n\t\t");
    content.append(post.toHtml());
    content.append(String("\n\t</body>\n\n</html>\n"));

    std::ofstream file(name.c_str(), std::ios::out | std::ios::trunc);
    if (file.good()) {
        file << content.c_str();
        return file.good();
    }

    return false;
}

PostRepository& PostRepository::instance() {
    static PostRepository post_repository;
    return post_repository;
}
