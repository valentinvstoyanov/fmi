#ifndef  __LINE_HEADER_INCLUDED__
#define __LINE_HEADER_INCLUDED__
#include <fstream>

class Line {
    char* content;
    unsigned length;
public:
    Line();
    Line(const char*);
    Line(const Line&);
    ~Line();
    Line& operator=(const Line&);
    void set_content(const char*);
    unsigned get_length() const;
    void print(std::ostream&) const;
};

#endif
