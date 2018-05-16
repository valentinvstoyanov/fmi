//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_USER_H
#define FMIBOOK_USER_H


class User {
    char* nickname_;
    unsigned age_;
public:
    User();
    User(const char* nickname, const unsigned age);
    User(const User&);
    ~User();
    User& operator=(const User&);
    void set_nickname(const char*);
    void set_age(const unsigned);
    unsigned get_age() const;
    const char* get_nickname() const;
};


#endif //FMIBOOK_USER_H
