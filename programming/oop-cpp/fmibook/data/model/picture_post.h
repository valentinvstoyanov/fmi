//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_PICTURE_POST_H
#define FMIBOOK_PICTURE_POST_H


#include "post.h"

class PicturePost: public Post {
public:
    explicit PicturePost(const char* = nullptr);
    char* toHtml() const override;
};


#endif //FMIBOOK_PICTURE_POST_H
