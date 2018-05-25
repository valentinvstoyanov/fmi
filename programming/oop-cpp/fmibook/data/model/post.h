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
 protected:
  enum Type { kLink, kPicture, kText };
  virtual Type get_type() const = 0;
 public:
  Post();
  explicit Post(const String&, const unsigned = 0);
  Post(const Post&) = default;
  virtual ~Post() = default;
  Post &operator=(const Post&);
  void set_content(const String&);
  void set_id(const unsigned);
  unsigned get_id() const;
  const String& get_content() const;
  virtual void serialize(std::ostream&) const;
  static Post* deserialize(std::istream&);
  virtual Post* create(std::istream&) const{};
  virtual String toHtml() const = 0;
  virtual Post* clone() const = 0;
};

#endif //FMIBOOK_POST_H
