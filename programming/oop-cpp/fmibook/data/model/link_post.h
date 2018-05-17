//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_LINK_POST_H
#define FMIBOOK_LINK_POST_H


#include "post.h"

class LinkPost: public Post{
    String description_;
public:
    LinkPost();
    LinkPost(const String&, const String&, const unsigned);
    LinkPost(const LinkPost&);
    ~LinkPost() override;
    LinkPost& operator=(const LinkPost&);
    void set_description(const String&);
    const String& get_description() const;
    String toHtml() const override;
};


#endif //FMIBOOK_LINK_POST_H
