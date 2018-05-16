//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_LINK_POST_H
#define FMIBOOK_LINK_POST_H


#include "post.h"

class LinkPost: public Post{
    char* description_;
public:
    LinkPost();
    LinkPost(const char*, const char*);
    LinkPost(const LinkPost&);
    ~LinkPost() override;
    LinkPost& operator=(const LinkPost&);
    void set_description(const char*);
    const char* get_description() const;
    char *toHtml() const override;
};


#endif //FMIBOOK_LINK_POST_H
