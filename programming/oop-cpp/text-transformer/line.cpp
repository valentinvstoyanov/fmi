#include <cstring>
#include <stdexcept>
#include <iostream>
#include "line.h"

Line::Line()
: content(nullptr),
  length(0)
{}

Line::Line(const char* new_content)
: content(nullptr),
  length(0)
{ set_content(new_content); }

Line::Line(const Line& other) {
    if(!other.content) {
        content = nullptr;
        length = 0;
    } else {
        length = other.length;
        content = new char[length + 1];
        strcpy(content, other.content);
    }
}

Line::~Line() {
    delete[] content;
    content = nullptr;
}

Line& Line::operator=(const Line& other) {
    if(this != &other) {
        delete[] content;
        if(!other.content) {
            content = nullptr;
            length = 0;
        } else {
            length = other.length;
            content = new char[length + 1];
            strcpy(content, other.content);
        }
    }
    return *this;
}

void Line::set_content(const char* new_content) {
    delete[] content;
    if(!new_content) {
        content = nullptr;
        length = 0;
    } else {
        length = strlen(new_content);
        content = new char[length + 1];
        strcpy(content, new_content);
    }
}

unsigned Line::get_length() const {
    return length;
}

char* Line::get_content() const {
    char* res = new char[length + 1];
    strcpy(res, content);
    return res;
}

void Line::print(std::ostream& stream/*  = std::cout */) const {
    if(stream && content)
        stream << content << "\n";
}
/*
void Line::f() {

}

void Line::format(const char* symbols, const bool bilateral) {
    const unsigned symbols_length = strlen(symbols);
    const unsigned result_length = length + (bilateral ? 2 * symbols_length + 2 : symbols_length + 1);
    char* result_str = new char[result_length + 1];
    strcpy(result_str, symbols);
    strcat(result_str, " ");
    strcat(result_str, content);
    if(bilateral) {
        strcat(result_str, " ");
        strcat(result_str, symbols);
    }
    delete[] content;
    content = result_str;
    length = result_length;
    result_str = nullptr;
}
*/
