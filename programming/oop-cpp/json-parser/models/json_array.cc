//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_array.h"

JsonArray::~JsonArray() {
  for (size_t i = 0; i < elements_.Size(); ++i)
    delete elements_[i];
}

void JsonArray::PushBack(JsonValue* value) {
  elements_.PushBack(value);
}
