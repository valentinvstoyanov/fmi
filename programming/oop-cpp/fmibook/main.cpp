#include <iostream>
#include "data/repository/post_repository.h"
#include "data/model/link_post.h"
#include "data/model/text_post.h"
#include "data/repository/user_repository.h"

int main() {

    String fn("random-name");
    String link("https://www.youtube.com/watch?v=y06RDyNXqbM");
    String descr("YouTube song");

    String a = String::fromInt(1234);
    a.reverse();
    std::cout << a.c_str() << std::endl;

    User user("test_user", 20);

    TextPost text_post(descr, PostRepository::next_id());
    UserRepository::instance().add_post(user, text_post);
    String name(user.get_nickname());
    PostRepository::instance().save_post(name, text_post);


    return 0;
}