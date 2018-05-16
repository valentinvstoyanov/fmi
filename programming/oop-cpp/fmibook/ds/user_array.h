//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_USER_ARRAY_H
#define FMIBOOK_USER_ARRAY_H


#include <cstddef>
#include "../data/model/user.h"

class UserArray {
    User* arr_;
    size_t  size_;
    size_t capacity_;
    void ensure_capacity(const size_t);
public:
    explicit UserArray(const size_t = 1);
    UserArray(const UserArray&);
    ~UserArray();
    UserArray& operator=(const UserArray&);
    void clear();
    void push_back(const User&);
    void pop_back();
    User& at(const size_t);
    const User& at(const size_t) const;
    const User& front() const;
    User& front();
    const User& back() const;
    User& back();
    bool empty() const;
    size_t size() const;
    size_t capacity() const;
};


#endif //FMIBOOK_USER_ARRAY_H
