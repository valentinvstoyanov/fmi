#include <cstring>
#include <stdexcept>
#include <iostream>
#include "line.h"

Line::Line()
:  content(nullptr),
   length(0)
{}

Line::Line(const char* new_content)
: content(nullptr),
  length(0)
{
    set_content(new_content);
}

Line::Line(const Line& other) {
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

void Line::print(std::ostream& stream) const {
    stream << (content ? content : "") << "\n";
}
