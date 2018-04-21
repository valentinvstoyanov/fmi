#ifndef __FORMATTER_HEADER_INCLUDED__
#define __FORMATTER_HEADER_INCLUDED__
#include "line.h"

class Formatter {
    char* delimeters;
    bool is_delimeter(const char& c) const;
    int get_nth_word_index(const char* str, const unsigned n, const bool = false) const;
    char* format(const char*, const char*, const bool) const;
    void format(Line&, const char*, const bool, const unsigned, const unsigned) const;
public:
    Formatter(const char* = " \t");
    Formatter(const Formatter&);
    Formatter& operator=(const Formatter&);
    ~Formatter();
    Line make_heading(const Line&) const;
    Line make_bold(const Line&, const unsigned, const unsigned) const;
    Line make_italic(const Line&, const unsigned, const unsigned) const;
    Line make_combined(const Line&, const unsigned, const unsigned) const;
    char* change_filename_extension(const char*, const char*) const;
};

#endif
