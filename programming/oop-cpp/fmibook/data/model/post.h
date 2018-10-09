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

  void SetContent(const String&);
  void SetId(const unsigned);

  unsigned GetId() const;
  const String& GetContent() const;

  virtual void Serialize(std::ostream&) const;
  virtual void Deserialize(std::istream&);

  virtual String ToHtml() const = 0;
  virtual Post* Clone() const = 0;

  enum Type { kText, kLink, kPicture };
  virtual Type GetType() const = 0;
};

#endif //FMIBOOK_POST_H
