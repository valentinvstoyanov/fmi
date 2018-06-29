//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_array.h"
#include "../util/json_token.h"

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

