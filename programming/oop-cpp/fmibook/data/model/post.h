//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_POST_H
#define FMIBOOK_POST_H


#include <ostream>
#include "../../ds/mystring.h"

class Post {
    unsigned id_;
    String content_;
public:
    Post();
    explicit Post(const String&, const unsigned = 0);
    Post(const Post&) = default;
    virtual ~Post() = default;
    Post& operator=(const Post&);
    void set_content(const String&);
    void set_id(const unsigned);
    unsigned get_id() const;
    const String& get_content() const;
    virtual void serialize(std::ostream&) const;
    virtual void deserialize(std::istream&);
    virtual String toHtml() const = 0;
};


#endif //FMIBOOK_POST_H
