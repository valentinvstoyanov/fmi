#ifndef  __TEXT_HEADER_INCLUDED__
#define __TEXT_HEADER_INCLUDED__
#include <fstream>
#include <iostream>
#include "line.h"

class Text {
    Line* lines;
    unsigned size;
    unsigned capacity;
    void ensure_capacity(const unsigned);
public:
    Text(const unsigned = 0);
    Text(const Text&);
    Text& operator=(const Text&);
    ~Text();
    void append_line(const Line&);
    void set_line_at(const Line&, const unsigned);
    const Line& get_line_at(const unsigned);
    void remove_line(const unsigned);
    void print(std::ostream& = std::cout) const;
};

#endif
