//
// Created by valio_stoyanov on 10/30/18.
//

#ifndef CONGA_LINE_CONGA_LINE_H
#define CONGA_LINE_CONGA_LINE_H

#include <bits/exception.h>
#include <stdexcept>
#include "student.h"

class CongaLine {
  struct Node {
    Student student_;
    Node* next_;
    Node* prev_;
    Node();
    explicit Node(const Student& student);
  } head_;

  Node* FindNodeByStudentName(const std::string&) const;
  CongaLine NewCongaLineFromTheRestNodes(Node*);
  void Clear();
  void CopyConga(const CongaLine&);
 public:
  CongaLine();
  CongaLine(const CongaLine&);
  ~CongaLine();
  CongaLine& operator=(const CongaLine&);

  const Student& Back() const;
  const Student& Front() const;
  void PushBack(const Student&);
  void PopBack();
  void PopFront();
  CongaLine Remove(const std::string&);
  void Append(CongaLine&);

  bool Empty() const;


  friend std::ostream& operator<<(std::ostream&, const CongaLine&);

  class StudentNotFoundException : public std::runtime_error {
   public:
    explicit StudentNotFoundException(const std::string& msg);
  };
};


#endif //CONGA_LINE_CONGA_LINE_H
