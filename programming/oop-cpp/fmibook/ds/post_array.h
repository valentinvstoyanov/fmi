//
// Created by valio_stoyanov on 5/24/18.
//

#ifndef FMIBOOK_POST_ARRAY_H
#define FMIBOOK_POST_ARRAY_H

#include <cstddef>
#include "../data/model/post.h"

class PostArray {
  Post** arr_;
  size_t size_;
  size_t capacity_;
  void EnsureCapacity(const size_t);
 public:
  explicit PostArray(const size_t = 1);
  PostArray(const PostArray&);
  ~PostArray();
  PostArray& operator=(const PostArray&);
  void Clear();
  void PushBack(const Post&);
  void PopBack();
  Post& At(const size_t);
  const Post& At(const size_t) const;
  const Post& Front() const;
  Post& Front();
  const Post& Back() const;
  Post& Back();
  bool Empty() const;
  size_t Size() const;
  size_t Capacity() const;
  void removeAt(size_t);
};

#endif //FMIBOOK_POST_ARRAY_H
