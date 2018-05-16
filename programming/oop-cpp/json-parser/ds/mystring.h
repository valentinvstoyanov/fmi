//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_STRING_H
#define JSON_PARSER_STRING_H


#include <ostream>

class String {
    char* buffer_;
    size_t size_;
    size_t capacity_;
    void ensure_capacity(size_t);
public:
    String(const char*);
    String(const String&);
    ~String();
    String& operator=(const String&);
    friend std::ostream& operator<<(std::ostream&, const String&);
    bool operator==(const String& other) const;
    bool operator!=(const String& other) const;
    bool operator<(const String& other) const;
    bool operator>(const String& other) const;
    bool operator<=(const String& other) const;
    bool operator>=(const String& other) const;
    char& operator[](const size_t);
    void push_back(char);
    String& append (const String&);
    void clear();
    char& at(size_t);
    char& back();
    char& front();
    const char& front() const;
    const char& back() const;
    size_t length() const;
    bool empty() const;
    char& at(size_t) const;
};


#endif //JSON_PARSER_STRING_H
