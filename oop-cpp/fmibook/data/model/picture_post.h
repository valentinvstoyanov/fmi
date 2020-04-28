//
// Created by valio_stoyanov on 5/16/18.
//

#ifndef FMIBOOK_PICTURE_POST_H
#define FMIBOOK_PICTURE_POST_H

#include "post.h"

class PicturePost : public Post {
 public:
  PicturePost();
  explicit PicturePost(const String &, const unsigned = 0);

  String ToHtml() const override;
  Post* Clone() const override;

  Type GetType() const override;
};

#endif //FMIBOOK_PICTURE_POST_H
