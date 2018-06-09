//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_null.h"

JsonNull& JsonNull::Instance() {
  static JsonNull json_null;
  return json_null;
}

