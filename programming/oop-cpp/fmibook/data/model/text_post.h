//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_TEXT_POST_H
#define FMIBOOK_TEXT_POST_H


#include "post.h"

class TextPost: public Post {
public:
    explicit TextPost(const char* = nullptr);
    char* toHtml() const override;
};


#endif //FMIBOOK_TEXT_POST_H
