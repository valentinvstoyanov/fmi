//
// Created by valio_stoyanov on 10/30/18.
//

#include <cassert>
#include <ostream>
#include "conga_line.h"

CongaLine::Node::Node(const Student& student)
    : student_(student) {}

CongaLine::CongaLine()
    : head_(Node(Student("", "tu"))) {
  head_.next_ = &head_;
  head_.prev_ = &head_;
}

CongaLine::CongaLine(const CongaLine& other)
    : head_(Node(Student("", "tu"))) {
  head_.next_ = &head_;
  head_.prev_ = &head_;
  CopyConga(other);
}

CongaLine::~CongaLine() {
  Clear();
}

CongaLine& CongaLine::operator=(const CongaLine& other) {
  if (this != &other) {
    Clear();
    CopyConga(other);
  }
  return *this;
}

void CongaLine::PushBack(const Student& student) {
  assert(Empty() || student.HasToleranceTo(head_.prev_->student_)
             && "CongaLine: cannot push back student.");
  Node* node = new Node(student);

  node->prev_ = head_.prev_;
  node->next_ = &head_;

  head_.prev_->next_ = node;
  head_.prev_ = node;
}

bool CongaLine::Empty() const {
  return head_.next_ == &head_;
}

void CongaLine::PopBack() {
  assert(!Empty() && "PopBack called on empty CongaLine");
  Node* back = head_.prev_;
  back->prev_->next_ = &head_;
  head_.prev_ = back->prev_;
  delete back;
}

void CongaLine::PopFront() {
  assert(!Empty() && "PopFront called on empty CongaLine");
  Node* front = head_.next_;
  front->prev_->next_ = &head_;
  head_.next_ = front->next_;
  delete front;
}

CongaLine::Node* CongaLine::FindNodeByStudentName(const std::string& name) const {
  Node* current = head_.next_->next_;
  while (current->next_ != &head_) {
    if (current->student_.GetName() == name)
      return current;
    current = current->next_;
  }
  return nullptr;
}

CongaLine CongaLine::NewCongaLineFromTheRestNodes(Node* node) {
  CongaLine res;
  node->prev_ = &res.head_;
  res.head_.next_ = node;
  res.head_.prev_ = head_.prev_;
  head_.prev_->next_ = &res.head_;
  return res;
}

CongaLine CongaLine::Remove(const std::string& name) {
  assert(!Empty() && "Remove called on empty CongaLine");
  if (head_.next_->student_.GetName() == name) {
    PopFront();
    return CongaLine();
  } else if (head_.prev_->student_.GetName() == name) {
    PopBack();
    return CongaLine();
  }

  Node* node = FindNodeByStudentName(name);
  if (node == nullptr)
    throw StudentNotFoundException(name + " not found");

  CongaLine result = NewCongaLineFromTheRestNodes(node->next_);
  node->prev_->next_ = &head_;
  head_.prev_ = node->prev_;
  delete node;

  return result;
}

void CongaLine::Append(CongaLine& other) {
  assert(other.head_.next_->student_.HasToleranceTo(head_.prev_->student_)
             && "CongaLine: cannot append conga.");

  head_.prev_->next_ = other.head_.next_;
  other.head_.next_->prev_ = head_.prev_;

  other.head_.prev_->next_ = &head_;
  head_.prev_ = other.head_.prev_;

  other.head_.next_ = other.head_.prev_ = &other.head_;
}

void CongaLine::Clear() {
  while (!Empty())
    PopFront();
}

void CongaLine::CopyConga(const CongaLine& other) {
  if (other.Empty())
    return;
  Node* other_curr = other.head_.next_;
  while (other_curr != &other.head_) {
    PushBack(other_curr->student_);
    other_curr = other_curr->next_;
  }
}

const Student& CongaLine::Back() const {
  assert(!Empty() && "CongaLine: Back() called on empty conga line.");
  return head_.prev_->student_;
}

const Student& CongaLine::Front() const {
  assert(!Empty() && "CongaLine: Front() called on empty conga line.");
  return head_.next_->student_;
}

std::ostream& operator<<(std::ostream& out, const CongaLine& conga_line) {
  CongaLine::Node* current = conga_line.head_.next_;
  while (current != &conga_line.head_) {
    out << current->student_ << (current->next_ != &conga_line.head_ ? "-" : "");
    current = current->next_;
  }
  return out;
}

CongaLine::StudentNotFoundException::StudentNotFoundException(const std::string& msg)
    : runtime_error(msg) {}
