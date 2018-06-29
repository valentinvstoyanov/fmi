//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_object.h"
#include "../util/json_token.h"

JsonObject::Node::Node()
    : data_(Pair<String, JsonValue*>(String(nullptr), nullptr)),
      next_(nullptr),
      prev_(nullptr) {}

JsonObject::Node::Node(const Pair<String, JsonValue*>& data)
    : data_(data), next_(nullptr), prev_(nullptr) {}

JsonObject::JsonObject()
    : head_(new Node), size_(0) {
  head_->prev_ = head_;
  head_->next_ = head_;
}

JsonObject::~JsonObject() {
  Clear();
  delete head_;
}

void JsonObject::PushBack(const Pair<String, JsonValue*>& val) {
  Node* current_back = head_->prev_;
  Node* new_back = new Node(val);

  head_->prev_ = new_back;
  new_back->next_ = head_;

  current_back->next_ = new_back;
  new_back->prev_ = current_back;

  size_++;
}

void JsonObject::PopBack() {
  if (Empty())
    throw std::runtime_error("JsonObject: cannot pop back on empty list.");

  Node* current_back = head_->prev_;
  Node* new_back = current_back->prev_;

  head_->prev_ = new_back;
  new_back->next_ = head_;

  delete current_back->data_.Value();
  delete current_back;
  size_--;
}

void JsonObject::PushFront(const Pair<String, JsonValue*>& val) {
  Node* current_front = head_->next_;
  Node* new_front = new Node(val);

  head_->next_ = new_front;
  new_front->prev_ = head_;

  current_front->prev_ = new_front;
  new_front->next_ = current_front;

  size_++;
}

void JsonObject::PopFront() {
  if (Empty())
    throw std::runtime_error("JsonObject: cannot pop front on empty list.");

  Node* current_front = head_->next_;
  Node* new_front = current_front->next_;

  head_->next_ = new_front;
  new_front->next_ = head_;

  delete current_front->data_.Value();
  delete current_front;
  size_--;
}

bool JsonObject::Empty() const {
  return head_->next_ == head_;
}

void JsonObject::Clear() {
  while (!Empty())
    PopFront();

  size_ = 0;
  head_->next_ = head_;
  head_->prev_ = head_;
}

const Pair<String, JsonValue*>& JsonObject::Front() const {
  if (Empty())
    throw std::runtime_error("JsonObject: cannot get front node of empty list.");

  return head_->next_->data_;
}

const Pair<String, JsonValue*>& JsonObject::Back() const {
  if (Empty())
    throw std::runtime_error("JsonObject: cannot get back node of empty list.");

  return head_->prev_->data_;
}

Pair<String, JsonValue*>& JsonObject::Front() {
  if (Empty())
    throw std::runtime_error("JsonObject: cannot get front node of empty list.");

  return head_->next_->data_;
}

Pair<String, JsonValue*>& JsonObject::Back() {
  if (Empty())
    throw std::runtime_error("JsonObject: cannot get back node of empty list.");

  return head_->prev_->data_;
}

size_t JsonObject::Size() const {
  return size_;
}

void JsonObject::Serialize(std::ostream& out, bool pretty, unsigned depth) const {
  out << JsonToken::kBegObjCh;

  const unsigned next_depth = depth + 1;
  CIterator it = CBegin();
  CIterator end_it = CEnd();
  for (size_t c = 1; it != end_it; ++it, ++c) {
    const Pair<String, JsonValue*>& pair = *it;
    if (pretty)
      WritePrettyLine(out, depth);
    out << JsonToken::kStrCh << pair.Key() << JsonToken::kStrCh
        << JsonToken::kColonSeparatorCh;
    pair.Value()->Serialize(out, pretty, next_depth);
    if (c != size_)
      out << JsonToken::kValueSeparatorCh;
  }

  if (pretty)
    WritePrettyLine(out, depth - 1);
  out << JsonToken::kEndObjCh;
}

