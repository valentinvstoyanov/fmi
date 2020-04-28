//
// Created by valio_stoyanov on 12/12/18.
//

#ifndef CHORBADJIISKO_HORO_DANCER_H
#define CHORBADJIISKO_HORO_DANCER_H

#include <string>
#include "hold.h"

struct Dancer {
  std::string label_;
  std::string left_label_;
  std::string right_label_;

  bool holds_left_;
  bool holds_right_;

  Dancer() = default;
  Dancer(const std::string&,
         const std::string&,
         const std::string&,
         bool = true,
         bool = true);

  bool HoldsSomeone() const;
  bool HoldsBoth() const;

  void SetHoldsBoth(bool);
};

#endif //CHORBADJIISKO_HORO_DANCER_H
