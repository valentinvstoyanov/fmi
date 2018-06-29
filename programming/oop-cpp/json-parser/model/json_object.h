//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_OBJECT_H
#define JSON_PARSER_JSON_OBJECT_H

#include "json_value.h"
#include "../ds/pair.h"

class JsonObject : public JsonValue {
  struct Node {
    Pair<String, JsonValue*> data_;
    Node* next_;
    Node* prev_;
    Node();
    explicit Node(const Pair<String, JsonValue*>&);
  };

  Node* head_;
  size_t size_;
  static String DeserializeKey(const char*&);
 public:
  JsonObject();
  JsonObject(const JsonObject& other) = delete;
  ~JsonObject() override;
  JsonObject& operator=(const JsonObject&) = delete;
  void PushBack(const Pair<String, JsonValue*>&);
  void PopBack();
  bool Empty() const;
  size_t Size() const;
  void Clear();
  void PopFront();
  void PushFront(const Pair<String, JsonValue*>&);
  const Pair<String, JsonValue*>& Front() const;
  const Pair<String, JsonValue*>& Back() const;
  Pair<String, JsonValue*>& Front();
  Pair<String, JsonValue*>& Back();

  void Serialize(std::ostream& out, bool pretty, unsigned depth) const override;
  static JsonObject* Deserialize(const char*&);

  class CIterator {
    Node* current_node_;
    CIterator(Node* current_node) : current_node_(current_node) {}
    friend class JsonObject;
   public:
    const Pair<String, JsonValue*>& operator*() {
      return current_node_->data_;
    }

    const CIterator& operator++() {
      current_node_ = current_node_->next_;
      return *this;
    }

    CIterator operator++(int) {
      CIterator res(*this);
      current_node_ = current_node_->next_;
      return res;
    }

    bool operator==(const CIterator& other) const {
      return current_node_ == other.current_node_;
    }

    bool operator!=(const CIterator& other) const {
      return !operator==(other);
    }
  };


  CIterator CBegin() const {
    return CIterator(head_->next_);
  }

  CIterator CEnd() const {
    return CIterator(head_);
  }
};

#endif //JSON_PARSER_JSON_OBJECT_H
