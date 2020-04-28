//
// Created by valio_stoyanov on 12/12/18.
//

#include <cstring>
#include "dancer.h"

Dancer::Dancer(const std::string& label,
               const std::string& left_label,
               const std::string& right_label,
               bool holds_left,
               bool holds_right)
    : label_(label),
      left_label_(left_label),
      right_label_(right_label),
      holds_left_(holds_left),
      holds_right_(holds_right) {}

bool Dancer::HoldsSomeone() const {
  return holds_left_ || holds_right_;
}

bool Dancer::HoldsBoth() const {
  return holds_left_ && holds_right_;
}

void Dancer::SetHoldsBoth(bool b) {
  holds_left_ = holds_right_ = b;
}

