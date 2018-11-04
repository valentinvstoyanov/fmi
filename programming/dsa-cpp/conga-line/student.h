//
// Created by valio_stoyanov on 10/30/18.
//

#ifndef CONGA_LINE_STUDENT_H
#define CONGA_LINE_STUDENT_H

#include <string>

class Student {
 public:
  enum University { kFmi, kUnss, kTu };
 private:
  std::string name_;
  University uni_;
 public:
  Student(const std::string&, University uni);
  Student(const std::string&, const std::string&);
  bool HasToleranceTo(const Student&) const;
  const std::string& GetName() const;
  friend std::ostream& operator<<(std::ostream&, const Student&);

 class NoSuchUniException : public std::runtime_error {
  public:
   explicit NoSuchUniException(const std::string& msg);
 };
};

#endif //CONGA_LINE_STUDENT_H
