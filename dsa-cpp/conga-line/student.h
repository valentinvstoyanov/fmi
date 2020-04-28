//
// Created by valio_stoyanov on 10/30/18.
//

#ifndef CONGA_LINE_STUDENT_H
#define CONGA_LINE_STUDENT_H

#include <string>

class Student {
  std::string name_;
  std::string uni_;
 public:
  Student(const std::string&, const std::string&);
  bool HasToleranceTo(const Student&) const;
  const std::string& GetName() const;
  friend std::ostream& operator<<(std::ostream&, const Student&);
};

#endif //CONGA_LINE_STUDENT_H
