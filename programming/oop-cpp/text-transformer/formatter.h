#ifndef __FORMATTER_HEADER_INCLUDED__
#define __FORMATTER_HEADER_INCLUDED__
#include "line.h"

class Formatter {
    char* delimeters;
    bool is_delimeter(const char& c) const;
    int get_nth_word_index(const char* str, const unsigned n, const bool = false) const;
    char* format(const char*, const char*, const bool) const;
    void format(Line& line, const char* symbols, const bool bilateral, const unsigned from, const unsigned to) const;
public:
    Formatter(const char* = " \t");
    Formatter(const Formatter&);
    Formatter& operator=(const Formatter&);
    ~Formatter();
    Line make_heading(const Line& line) const;
    Line make_bold(const Line& line, const unsigned from, const unsigned to) const;
    Line make_italic(const Line& line, const unsigned from, const unsigned to) const;
    Line make_combined(const Line& line, const unsigned from, const unsigned to) const;
};

#endif
