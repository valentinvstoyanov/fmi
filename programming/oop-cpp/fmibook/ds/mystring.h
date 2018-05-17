//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_MYSTRING_H
#define FMIBOOK_MYSTRING_H


#include <cstddef>
#include <ostream>
#include <istream>

class String {
    char *buffer_;
    size_t size_;
    size_t capacity_;
    void ensure_capacity(const size_t);
public:
    explicit String(const char*);
    String(const String&);
    ~String();
    String& operator=(const String&);
    void push_back(char);
    String& append(const String&);
    void clear();
    char& at(size_t);
    char& back();
    char& front();
    void reverse();
    const char* c_str() const;
    const char& front() const;
    const char& back() const;
    size_t length() const;
    bool empty() const;
    char& at(size_t) const;
    void serialize(std::ostream&) const;
    void deserialize(std::istream&);
    static String fromInt(int);
};


#endif //FMIBOOK_MYSTRING_H
