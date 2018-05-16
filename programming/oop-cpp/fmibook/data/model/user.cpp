//
// Created by valio_stoyanov on 5/16/18.
//

#include <cstring>
#include "user.h"

User::User()
: nickname_(nullptr), age_(0)
{}

User::User(const char* nickname, const unsigned age)
: nickname_(nullptr), age_(age)
{ set_nickname(nickname); }

User::User(const User& user)
: nickname_(nullptr), age_(user.age_)
{ set_nickname(user.nickname_); }

User::~User() {
    delete[] nickname_;
    nickname_ = nullptr;
}

User& User::operator=(const User& other) {
    if (this != &other) {
        age_ = other.age_;
        set_nickname(other.nickname_);
    }

    return *this;
}

void User::set_nickname(const char* nickname) {
    delete[] nickname_;
    nickname_ = nullptr;
    if (nickname != nullptr) {
        nickname_ = new char[strlen(nickname) + 1];
        strcpy(nickname_, nickname);
    }
}

void User::set_age(const unsigned age) {
    age_ = age;
}

unsigned User::get_age() const {
    return age_;
}

const char* User::get_nickname() const {
    return nickname_;
}


