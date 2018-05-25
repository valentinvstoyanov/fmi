//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_LINK_POST_H
#define FMIBOOK_LINK_POST_H

#include "post.h"

class LinkPost : public Post {
  String description_;
 public:
  LinkPost();
  LinkPost(const String&, const String&, const unsigned = 0);
  LinkPost(const LinkPost&);
  virtual ~LinkPost() override;
  LinkPost &operator=(const LinkPost&);
  void set_description(const String&);
  const String& get_description() const;
  String toHtml() const override;
  Post* clone() const override;
 protected:
  Type get_type() const override;
};

#endif //FMIBOOK_LINK_POST_H
