#include "json_number.h"
#include "../exception/deserialize_exception.h"

JsonNumber::JsonNumber(long long value)
    : is_real_(false), value_{.integer_ = value} {}

JsonNumber::JsonNumber(double value)
    : is_real_(true), value_{.real_ = value} {}

bool JsonNumber::IsReal() const {
  return is_real_;
}

bool JsonNumber::IsInteger() const {
  return !is_real_;
}

long long JsonNumber::GetInteger() const {
  if (is_real_) return static_cast<long long int>(value_.real_);
  else return value_.integer_;
}

double JsonNumber::GetReal() const {
  return is_real_ ? value_.real_ : value_.integer_;
}

void JsonNumber::Serialize(std::ostream& out, bool pretty, unsigned depth) const {
  if (is_real_) out << value_.real_;
  else out << value_.integer_;
}

JsonNumber* JsonNumber::Deserialize(const char*& str) {
  throw DeserializeException("Number deserialization is not yet implemented.");
}
JsonValue* JsonNumber::Clone() const {
  return new JsonNumber(*this);
}


