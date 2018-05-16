//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_ABSTRACT_POST_H
#define FMIBOOK_ABSTRACT_POST_H


class Post {
    char* content_;
public:
    explicit Post(const char* = nullptr);
    Post(const Post&);
    virtual ~Post();
    Post& operator=(const Post&);
    void set_content(const char*);
    const char* get_content() const;
    virtual char* toHtml() const = 0;
};


#endif //FMIBOOK_ABSTRACT_POST_H
