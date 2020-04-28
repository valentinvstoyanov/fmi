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
  ~LinkPost() override;
  LinkPost &operator=(const LinkPost&);

  void SetDescription(const String&);

  const String& GetDescription() const;

  String ToHtml() const override;
  Post* Clone() const override;
  Type GetType() const override;

  void Serialize(std::ostream& ostream) const override;
  void Deserialize(std::istream& istream) override;
};

#endif //FMIBOOK_LINK_POST_H
