//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_null.h"

JsonNull& JsonNull::instance() const {
    static JsonNull json_null;
    return json_null;
}

