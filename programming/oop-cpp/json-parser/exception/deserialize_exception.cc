//
// Created by valio_stoyanov on 6/29/18.
//

#include "deserialize_exception.h"

DeserializeException::DeserializeException(const char* message)
    : runtime_error(""), message_("DeserializeException: ") {
  message_.Append(String(message));
}

DeserializeException::DeserializeException(const String& message)
    : runtime_error(""), message_("DeserializeException: ") {
  message_.Append(message);
}

const char* DeserializeException::what() const noexcept {
  return message_.CStr();;
}
