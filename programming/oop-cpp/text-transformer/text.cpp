#include <iostream>
#include "text.h"

unsigned max(unsigned a, unsigned b) {
    return (a > b ? a : b);
}

Text::Text(const unsigned initial_capacity/* = 0*/)
: size(0),
  capacity(initial_capacity),
  lines(nullptr)
{
    if(capacity > 0)
        lines = new Line[capacity];
}

Text::Text(const Text& other) {
    delete[] lines;
    size = other.size;
    capacity = other.capacity;
    lines = new Line[capacity];
    for(unsigned i = 0; i < size; ++i)
        lines[i] = other.lines[i];
}

Text::~Text() {
    delete[] lines;
    lines = nullptr;
}

Text& Text::operator=(const Text& other) {
    delete[] lines;
    size = other.size;
    capacity = other.capacity;
    lines = new Line[capacity];
    for(unsigned i = 0; i < size; ++i)
        lines[i] = other.lines[i];
}

void Text::ensure_capacity(const unsigned new_lines_count) {
    if(size + new_lines_count <= capacity)
        return;
    unsigned new_capacity = max(2 * capacity, size + new_lines_count);
    Line* old_lines = lines;
    lines = new Line[new_capacity];
    for(unsigned i = 0; i < size; ++i)
        lines[i] = old_lines[i];
    capacity = new_capacity;
    delete[] old_lines;
}

void Text::append_line(const Line& line) {
    ensure_capacity(1);
    lines[size++] = line;
}

void Text::set_line_content(const char* new_content, const unsigned index) {
    if(index < 0 || index >= size)
        return;
    lines[index].set_content(new_content);
}

void Text::remove_line(const unsigned index) {
    if(index < 0 || index >= size)
        return;
    Line* new_lines = new Line[size - 1];
    for(unsigned i = 0; i < index; ++i)
        new_lines[i] = lines[i];
    for(unsigned i = index + 1; i < size; ++i)
        new_lines[i] = lines[i];
    size--;
    capacity = size;
    delete[] lines;
    lines = new_lines;
    new_lines = nullptr;
}

void Text::print(std::ostream& stream) const {
    if(!lines)
        return;
    for(unsigned i = 0; stream && i < size; ++i)
        lines[i].print(stream);
}
