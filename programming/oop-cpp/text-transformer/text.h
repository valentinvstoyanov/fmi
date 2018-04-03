#ifndef  __TEXT_HEADER_INCLUDED__
#define __TEXT_HEADER_INCLUDED__
#include <fstream>
#include "line.h"

class Text {
    Line* lines;
    unsigned size;
    unsigned capacity;
    void ensure_capacity(const unsigned);
public:
    Text(const unsigned = 0);
    Text(const Text&);
    ~Text();
    Text& operator=(const Text&);
    void append_line(const Line&);
    void set_line_content(const char*, const unsigned);
    void remove_line(const unsigned);
    void print(std::ostream&) const;
};

#endif
