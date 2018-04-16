#include <stdexcept>
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

Text::Text(const Text& text)
: size(text.size), capacity(text.capacity) {
    if(capacity > 0) {
        lines = new Line[capacity];
        for(unsigned i = 0; i < size; ++i)
            lines[i] = text.lines[i];
    }
}

Text& Text::operator=(const Text& other) {
    if(this != &other) {
        delete[] lines;
        size = other.size;
        capacity = other.capacity;
        if(capacity > 0) {
            lines = new Line[capacity];
            for(unsigned i = 0; i < size; ++i)
                lines[i] = other.lines[i];
        }
    }
    return *this;
}

Text::~Text() {
    delete[] lines;
    lines = nullptr;
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

void Text::set_line_at(const Line& line, const unsigned index) {
    if(index >= size)
        return;
    lines[index] = line;
}

const Line& Text::get_line_at(const unsigned index) {
    if(index >= size)
        throw std::invalid_argument("No such index in lines");
    return lines[index];
}

void Text::remove_line(const unsigned index) {
    if(index >= size)
        return;
    Line* new_lines = new Line[size - 1];
    for(unsigned i = 0; i < index; ++i)
        new_lines[i] = lines[i];
    for(unsigned i = index + 1; i < size; ++i)
        new_lines[i - 1] = lines[i];
    size--;
    capacity = size;
    delete[] lines;
    lines = new_lines;
    new_lines = nullptr;
}

void Text::print(std::ostream& stream/*  = std::cout */) const {
    if(lines)
        for(unsigned i = 0; stream && i < size; ++i)
            lines[i].print(stream);
}
