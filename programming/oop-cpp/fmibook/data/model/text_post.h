//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_TEXT_POST_H
#define FMIBOOK_TEXT_POST_H


#include "post.h"

class TextPost: public Post {
public:
    TextPost();
    explicit TextPost(const String&, const unsigned = 0);
    String toHtml() const override;
};


#endif //FMIBOOK_TEXT_POST_H
