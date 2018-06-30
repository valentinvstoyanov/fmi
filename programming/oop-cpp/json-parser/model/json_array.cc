#include "json_array.h"
#include "../util/json_token.h"
#include "../exception/deserialize_exception.h"
#include "../util/cstr.h"

JsonArray::~JsonArray() {
  for (size_t i = 0; i < elements_.Size(); ++i)
    delete elements_[i];
}

void JsonArray::PushBack(JsonValue* value) {
  elements_.PushBack(value);
}

void JsonArray::Serialize(std::ostream& out, bool pretty, unsigned depth) const {
  out << JsonToken::kBegArrCh;
  const unsigned next_depth = depth + 1;
  for (int i = 0; i < elements_.Size() - 1; ++i) {
    if (pretty)
      WritePrettyLine(out, depth);
    elements_[i]->Serialize(out, pretty, next_depth);
    out << JsonToken::kValueSeparatorCh;
  }
  if (pretty)
    WritePrettyLine(out, depth);
  elements_[elements_.Size() - 1]->Serialize(out, pretty, next_depth);

  if (pretty)
    WritePrettyLine(out, depth - 1);
  out << JsonToken::kEndArrCh;
}

JsonArray* JsonArray::Deserialize(const char*& str) {
  if (*str != JsonToken::kBegArrCh) {
    String err_msg("Missing opening array bracket > ");
    err_msg.Append(String(str));
    throw DeserializeException(err_msg);
  }

  JsonArray* json_arr = new JsonArray();

  if (*StrSkipWhiteSpace(str + 1) == JsonToken::kEndArrCh) {
    ++str;
    return json_arr;
  }

  do {
    ++str;
    str = StrSkipWhiteSpace(str);
    JsonValue* value = JsonValue::FromJson(str);
    json_arr->PushBack(value);
    str = StrSkipWhiteSpace(str);
  } while (*str == JsonToken::kValueSeparatorCh);

  str = StrSkipWhiteSpace(str);

  if (*str != JsonToken::kEndArrCh) {
    String err_msg("Missing closing array bracket > ");
    err_msg.Append(String(str));
    throw DeserializeException(err_msg);
  }

  ++str;

  return json_arr;
}

