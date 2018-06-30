#include "json_value.h"
#include "../util/cstr.h"
#include "../util/json_token.h"
#include "json_null.h"
#include "json_boolean.h"
#include "json_string.h"
#include "json_array.h"
#include "json_number.h"
#include "json_object.h"
#include "../exception/deserialize_exception.h"

void JsonValue::WritePrettyLine(std::ostream& out, unsigned depth) const {
  out << '\n';
  for (unsigned i = 0; i < depth; ++i)
    out << "    ";
}

void JsonValue::Serialize(std::ostream& out, bool pretty) const {
  Serialize(out, pretty, 1);
}

JsonValue* JsonValue::FromJson(const char*& str) {
  str = StrSkipWhiteSpace(str);
  if (*str == '\0')
    throw DeserializeException("Cannot parse json value from empty string.");

  if (*str == JsonToken::kStrCh)
    return JsonString::Deserialize(str);
  else if (*str == JsonToken::kBegArrCh)
    return JsonArray::Deserialize(str);
  else if (*str == '-' || IsDigit(*str))
    return JsonNumber::Deserialize(str);
  else if (*str == *JsonToken::kNullStr)
    return JsonNull::Deserialize(str);
  else if (*str == *JsonToken::kTrueStr || *str == *JsonToken::kFalseStr)
    return JsonBoolean::Deserialize(str);
  else if (*str== JsonToken::kBegObjCh)
    return JsonObject::Deserialize(str);

  String err_msg("Unknown value type found.Check for missing symbols like {, [, \" > ");
  err_msg.Append(String(str));
  throw DeserializeException(err_msg);
}
