#ifndef  __LINE_HEADER_INCLUDED__
#define __LINE_HEADER_INCLUDED__
#include <fstream>
#include <iostream>

class Line {
    char* content;
    unsigned length;
public:
    Line();
    Line(const char*);
    Line(const Line&);
    Line& operator=(const Line&);
    ~Line();
    void set_content(const char*);
    unsigned get_length() const;
    char* get_content() const;
    void print(std::ostream& = std::cout) const;
};

#endif
