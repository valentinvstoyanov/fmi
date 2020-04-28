//
// Created by valio_stoyanov on 11/3/18.
//

#ifndef CONGA_LINE_CONGA_COLLECTION_H
#define CONGA_LINE_CONGA_COLLECTION_H

#include "conga_line.h"

class CongaCollection {
  CongaLine* conga_lines_;
  size_t size_;
  size_t capacity_;
  void Clear();
  void CopyCongaCollection(const CongaCollection&);
 public:
  explicit CongaCollection(size_t = 2);
  CongaCollection(const CongaCollection&);
  ~CongaCollection();

  CongaCollection& operator=(const CongaCollection&);

  size_t Size() const;
  bool Empty() const;

  void PushBack(const CongaLine&);

  long Append(const Student&, size_t);
  void RemoveLast(size_t);
  void RemoveFirst(size_t);
  void Remove(const std::string&, size_t);
  bool Merge(size_t, size_t);

  friend std::ostream& operator<<(std::ostream&, const CongaCollection&);
};

#endif //CONGA_LINE_CONGA_COLLECTION_H
