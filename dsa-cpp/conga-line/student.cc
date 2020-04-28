//
// Created by valio_stoyanov on 10/30/18.
//

#include <ostream>
#include <cassert>
#include "student.h"

Student::Student(const std::string& name, const std::string& uni)
    : name_(name), uni_(uni) {
  assert((uni == "fmi" || uni == "unss" || uni == "tu") && "Unknown university");
}

bool Student::HasToleranceTo(const Student& student) const {
  return uni_ == student.uni_ ||
      (uni_ == "fmi" && student.uni_ == "tu") ||
      (uni_ == "tu" && student.uni_ == "unss") ||
      (uni_ == "unss" && student.uni_ == "fmi");
}

const std::string& Student::GetName() const {
  return name_;
}

std::ostream& operator<<(std::ostream& out, const Student& student) {
  return out << '(' << student.name_ << ", " << student.uni_ << ')';
}