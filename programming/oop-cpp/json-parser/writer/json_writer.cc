//
// Created by valio_stoyanov on 6/29/18.
//

#include <iostream>
#include "json_writer.h"
#include "../util/json_token.h"

JsonWriter::JsonWriter(bool pretty /*= false*/)
    : pretty_(pretty) {}

void JsonWriter::SetPretty(bool pretty) {
  pretty_ = pretty;
}

bool JsonWriter::IsPretty() const {
  return pretty_;
}

void JsonWriter::Write(std::ostream& out, const JsonValue& obj) {
  obj.Serialize(out, pretty_);
}


