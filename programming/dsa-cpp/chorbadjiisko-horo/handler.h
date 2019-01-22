//
// Created by valio_stoyanov on 12/14/18.
//

#ifndef CHORBADJIISKO_HORO_HANDLER_H
#define CHORBADJIISKO_HORO_HANDLER_H

#include "dance.h"

class Handler {
  Dance dance_;
 public:
  explicit Handler(const Dance&);

  void Release(const std::string&, Hold);
  void Grab(const std::string&, Hold);
  void Info(const std::string&) const;
  void Add(const std::string&, const std::string&, const std::string&);
  void Remove(const std::string&);
  void Swap(const std::string&, const std::string&);
  void Print() const;
  void Exit() const;
  void InputErr(const std::string&) const;
};

#endif //CHORBADJIISKO_HORO_HANDLER_H
