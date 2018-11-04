//
// Created by valio_stoyanov on 10/30/18.
//

#include <ostream>
#include "student.h"

Student::Student(const std::string& name, University uni)
    : name_(name), uni_(uni) {}

Student::Student(const std::string& name, const std::string& uni)
    : name_(name) {
  if (uni == "fmi") uni_ = kFmi;
  else if (uni == "unss") uni_ = kUnss;
  else if (uni == "tu") uni_ = kTu;
  else throw NoSuchUniException(uni + ": invalid university.");
}

bool Student::HasToleranceTo(const Student& student) const {
  return uni_ == student.uni_ ||
      (uni_ == kFmi && student.uni_ == kTu) ||
      (uni_ == kTu && student.uni_ == kUnss) ||
      (uni_ == kUnss && student.uni_ == kFmi);
}

const std::string& Student::GetName() const {
  return name_;
}

std::ostream& operator<<(std::ostream& out, const Student& student) {
  out << '(' << student.name_ << ", ";
  switch (student.uni_) {
    case Student::kFmi: out << "fmi";
      break;
    case Student::kUnss: out << "unss";
      break;
    case Student::kTu: out << "tu";
      break;
  }
  return out << ')';
}

Student::NoSuchUniException::NoSuchUniException(const std::string& msg)
    : runtime_error(msg) {}
