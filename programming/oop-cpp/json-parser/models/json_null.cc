//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_null.h"

std::ostream& operator<<(std::ostream& out, const JsonNull&) {
  return out << "null";
}
