#include "json_object.h"
#include "../util/json_token.h"
#include "../util/cstr.h"
#include "../exception/deserialize_exception.h"

JsonObject::Node::Node()
    : data_(Pair<String, JsonValue*>(String(nullptr), nullptr)),
      next_(nullptr),
      prev_(nullptr) {}

JsonObject::Node::Node(const Pair<String, JsonValue*>& data)
    : data_(data.Key(),
            data.Value() == nullptr ? nullptr : data.Value()->Clone()),
      next_(nullptr),
      prev_(nullptr) {}

JsonObject::Node::~Node() {
  delete data_.Value();
  next_ = nullptr;
  prev_ = nullptr;
}

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
  if (Exists(val.Key())) {
    String msg(val.Key());
    msg.Append(String(" already exists in JsonObject."));
    throw KeyAlreadyExistsExeption(msg);
  }

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

  delete current_back;
  size_--;
}

void JsonObject::PushFront(const Pair<String, JsonValue*>& val) {
  if (Exists(val.Key())) {
    String msg(val.Key());
    msg.Append(String(" already exists in JsonObject."));
    throw KeyAlreadyExistsExeption(msg);
  }

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

String JsonObject::DeserializeKey(const char*& str) {
  if (*str != JsonToken::kStrCh) {
    String err_msg("Missing opening quotation mark for key  > ");
    err_msg.Append(String(str));
    throw DeserializeException(err_msg);
  }

  const int closing_quot_mark_index = StrIndexOf(str + 1, JsonToken::kStrCh) + 1;
  if (closing_quot_mark_index < 0) {
    String err_msg("Missing closing quotation mark for key  > ");
    err_msg.Append(String(str));
    throw DeserializeException(err_msg);
  }

  String result(static_cast<size_t>(closing_quot_mark_index - 1));
  for (unsigned i = 1; i < closing_quot_mark_index; ++i)
    result.PushBack(str[i]);

  return result;
}

JsonObject* JsonObject::Deserialize(const char*& str) {
  str = StrSkipWhiteSpace(str);
  if (*str != JsonToken::kBegObjCh) {
    String err_msg("Missing opening object bracket > ");
    err_msg.Append(String(str));
    throw DeserializeException(err_msg);
  }

  JsonObject* json_obj = new JsonObject;

  if (*StrSkipWhiteSpace(str + 1) == JsonToken::kEndObjCh) {
    ++str;
    return json_obj;
  }

  do {
    ++str;
    str = StrSkipWhiteSpace(str);
    String key = DeserializeKey(str);
    str += key.Length() + 2;
    str = StrSkipWhiteSpace(str);
    if (*str != JsonToken::kColonSeparatorCh) {
      String err_msg("Missing colon between key and value > ");
      err_msg.Append(String(str));
      throw DeserializeException(err_msg);
    }
    ++str;
    JsonValue* value = JsonValue::FromJson(str);
    json_obj->PushBack(Pair<String, JsonValue*>(key, value));
    str = StrSkipWhiteSpace(str);
  } while (*str == JsonToken::kValueSeparatorCh);

  str = StrSkipWhiteSpace(str);

  if (*str != JsonToken::kEndObjCh) {
    String err_msg("Missing closing object bracket > ");
    err_msg.Append(String(str));
    throw DeserializeException(err_msg);
  }

  ++str;

  return json_obj;
}

JsonValue* JsonObject::GetValueByKey(const String& key) const {
  CIterator it = CBegin();
  CIterator end_it = CEnd();
  for (; it != end_it; ++it) {
    const Pair<String, JsonValue*>& pair = *it;
    if (pair.Key() == key)
      return pair.Value()->Clone();
  }
  return nullptr;
}

JsonValue* JsonObject::GetNonClonedValueByKey(const String& key) {
  CIterator it = CBegin();
  CIterator end_it = CEnd();
  for (; it != end_it; ++it) {
    const Pair<String, JsonValue*>& pair = *it;
    if (pair.Key() == key)
      return pair.Value();
  }
  return nullptr;
}

JsonObject* JsonObject::Clone() const {
  return new JsonObject(*this);
}

JsonObject::JsonObject(const JsonObject& other)
    : head_(new Node), size_(0) {
  head_->prev_ = head_;
  head_->next_ = head_;
  CIterator it = other.CBegin();
  CIterator end_it = other.CEnd();
  for (; it != end_it; ++it)
    PushBack(*it);
  size_ = other.size_;
}

JsonObject& JsonObject::operator=(const JsonObject& other) {
  if (this != &other) {
    Clear();
    CIterator it = other.CBegin();
    CIterator end_it = other.CEnd();
    for (; it != end_it; ++it)
      PushBack(*it);
    size_ = other.size_;
  }

  return *this;
}

JsonArray JsonObject::GetValuesByKey(const String& key) const {
  JsonArray array;
  CIterator it = CBegin();
  CIterator end_it = CEnd();
  for (; it != end_it; ++it) {
    const Pair<String, JsonValue*>& pair = *it;
    if (pair.Key() == key) {
      JsonObject* obj = new JsonObject;
      obj->PushBack(Pair<String, JsonValue*>(key, pair.Value()->Clone()));
      array.PushBack(obj);
    }
    if (JsonObject* obj = dynamic_cast<JsonObject*>(pair.Value()))
      array.Append(obj->GetValuesByKey(key));
  }

  return array;
}

bool JsonObject::Exists(const String& key) const {
  CIterator it = CBegin();
  CIterator end_it = CEnd();
  for (; it != end_it; ++it) {
    const Pair<String, JsonValue*>& pair = *it;
    if (pair.Key() == key)
      return true;
  }

  return false;
}

void JsonObject::AddPair(const String& key, const char* json) {
  PushBack(Pair<String, JsonValue*>(key, JsonValue::FromJson(json)));
}

void JsonObject::CreateValueInPath(const Array<String>& keys, const char* json) {
  if (keys.Size() == 0)
    return;

  if (keys.Size() == 1) {
    PushBack(Pair<String, JsonValue*>(keys.Front(), JsonValue::FromJson(json)));
    return;
  }

  JsonValue* first_key_from_path_val = GetNonClonedValueByKey(keys.Front());
  if (first_key_from_path_val == nullptr) {
    String msg("Failed to find key in object: ");
    msg.Append(keys.Front());
    throw InvalidPathException(msg);
  }

  if (keys.Size() >= 2) {
    if (JsonObject* obj = dynamic_cast<JsonObject*>(first_key_from_path_val)) {
      Array<String> new_keys(keys.Size() - 1);
      for (int i = 1; i < keys.Size(); ++i)
        new_keys.PushBack(keys[i]);
      obj->CreateValueInPath(new_keys, json);
    } else {
      String msg(keys[1]);
      msg.Append(String("'s value is not JsonObject."));
      throw InvalidPathException(msg);
    }
  }
}

void JsonObject::CreateValueInPath(const String& path, const char* json) {
  CreateValueInPath(path.Split('/'), json);
}

void JsonObject::RemovePair(const String& key) {
  bool deleted = false;
  CIterator it = CBegin();
  CIterator end_it = CEnd();
  for (; it != end_it; ++it) {
    const Pair<String, JsonValue*>& pair = *it;
    if (pair.Key() == key) {
      Erase(it);
      return;
    }
  }
  if (!deleted) {
    String msg(key);
    msg.Append(String(" not found in object."));
    throw KeyNotExistsException(msg);
  }
}

void JsonObject::DeleteValueInPath(const Array<String>& keys) {
  if (keys.Size() == 0)
    return;

  if (keys.Size() == 1) {
    RemovePair(keys.Front());
    return;
  }

  JsonValue* first_key_from_path_val = GetNonClonedValueByKey(keys.Front());
  if (first_key_from_path_val == nullptr) {
    String msg("Failed to find key in object: ");
    msg.Append(keys.Front());
    throw InvalidPathException(msg);
  }

  if (keys.Size() >= 2) {
    if (JsonObject* obj = dynamic_cast<JsonObject*>(first_key_from_path_val)) {
      Array<String> new_keys(keys.Size() - 1);
      for (int i = 1; i < keys.Size(); ++i)
        new_keys.PushBack(keys[i]);
      obj->DeleteValueInPath(new_keys);
    } else {
      String msg(keys[1]);
      msg.Append(String("'s value is not JsonObject."));
      throw InvalidPathException(msg);
    }
  }
}

void JsonObject::DeleteValueInPath(const String& path) {
  DeleteValueInPath(path.Split('/'));
}

void JsonObject::ChangePair(const String& key, const char* json) {
  RemovePair(key);
  AddPair(key, json);
}

void JsonObject::ChangeValueInPath(const Array<String>& keys, const char* json) {
  DeleteValueInPath(keys);
  CreateValueInPath(keys, json);
}

void JsonObject::ChangeValueInPath(const String& path, const char* json) {
  ChangeValueInPath(path.Split('/'), json);
}

JsonObject& JsonObject::operator+=(const Pair<String, const char*>& pair) {
  AddPair(pair.Key(), pair.Value());
  return *this;
}
JsonObject& JsonObject::operator+=(const Pair<String, JsonValue*>& pair) {
  PushBack(pair);
  return *this;
}

JsonObject JsonObject::operator+(const Pair<String, const char*>& pair) const {
  JsonObject obj = *this;
  obj += pair;
  return obj;
}

JsonObject JsonObject::operator+(const Pair<String, JsonValue*>& pair) const {
  JsonObject obj = *this;
  obj += pair;
  return obj;
}

bool JsonObject::operator==(const JsonObject& other) const {
  if (size_ != other.size_)
    return false;

  CIterator this_it = CBegin();
  CIterator this_end_it = CEnd();
  CIterator other_it = CBegin();
  CIterator other_end_it = CEnd();
  for (; this_it != this_end_it && other_it != other_end_it;
         ++this_it, ++other_it) {
    if (this_it != other_it)
      return false;
  }

  return true;
}

bool JsonObject::operator!=(const JsonObject& obj) const {
  return !operator==(obj);
}

JsonValue* JsonObject::operator[](const String& key) const {
  return GetValueByKey(key);
}

