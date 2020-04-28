#ifndef JSON_PARSER_JSON_OBJECT_H
#define JSON_PARSER_JSON_OBJECT_H

#include "json_value.h"
#include "../ds/pair.h"
#include "json_array.h"

class JsonObject : public JsonValue {
  class Node {
   public:
    Pair<String, JsonValue*> data_;
    Node* next_;
    Node* prev_;
    Node();
    explicit Node(const Pair<String, JsonValue*>&);
    Node(const Node&) = delete;
    Node& operator=(const Node&) = delete;
    ~Node();
  };

  Node* head_;
  size_t size_;
  static String DeserializeKey(const char*&);
  JsonValue* GetNonClonedValueByKey(const String& key);
  void CreateValueInPath(const Array<String>& keys, const char* json);
  void DeleteValueInPath(const Array<String>& keys);
  void ChangeValueInPath(const Array<String>& keys, const char* json);
 public:
  JsonObject();
  JsonObject(const JsonObject& other);
  ~JsonObject() override;
  JsonObject& operator=(const JsonObject&);
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
  JsonValue* GetValueByKey(const String&) const;
  JsonArray GetValuesByKey(const String&) const;
  bool Exists(const String&) const;
  void AddPair(const String&, const char*);
  void RemovePair(const String&);
  void CreateValueInPath(const String&, const char*);
  void DeleteValueInPath(const String&);
  void ChangePair(const String&, const char* json);
  void ChangeValueInPath(const String&, const char* json);
  JsonObject& operator+=(const Pair<String, const char*>&);
  JsonObject& operator+=(const Pair<String, JsonValue*>& val);
  JsonObject operator+(const Pair<String, const char*>&) const;
  JsonObject operator+(const Pair<String, JsonValue*>& val) const;
  bool operator==(const JsonObject&) const;
  bool operator!=(const JsonObject&) const;
  JsonValue* operator[](const String& key) const;


  void Serialize(std::ostream& out, bool pretty, unsigned depth) const override;
  static JsonObject* Deserialize(const char*&);
  JsonObject* Clone() const override;

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

  CIterator Erase(CIterator it) {
    Node* current_node = it.current_node_;
    current_node->prev_->next_ = current_node->next_;
    current_node->next_->prev_ = current_node->prev_;
    Node* new_curr = current_node->next_;
    delete current_node;
    size_--;
    return CIterator(new_curr);
  }

  class KeyAlreadyExistsExeption : public std::runtime_error {
    String msg_;
   public:
    KeyAlreadyExistsExeption(const String& msg)
        : runtime_error(""), msg_(msg) {}
    const char* what() const noexcept override {
      return msg_.CStr();
    }
  };

  class KeyNotExistsException : public std::runtime_error {
    String msg_;
   public:
    KeyNotExistsException(const String& msg)
        : runtime_error(""), msg_(msg) {}
    const char* what() const noexcept override {
      return msg_.CStr();
    }
  };

  class InvalidPathException : public std::runtime_error {
    String msg_;
   public:
    InvalidPathException(const String& msg)
        : runtime_error(""), msg_(msg) {}
    const char* what() const noexcept override {
      return msg_.CStr();
    }
  };
};

#endif //JSON_PARSER_JSON_OBJECT_H
