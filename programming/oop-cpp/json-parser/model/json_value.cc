//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_value.h"

void JsonValue::WritePrettyLine(std::ostream& out, unsigned depth) const {
  out << '\n';
  for (unsigned i = 0; i < depth; ++i)
    out << "    ";
}

void JsonValue::Serialize(std::ostream& out, bool pretty) const {
  Serialize(out, pretty, 1);
}