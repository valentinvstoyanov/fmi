//
// Created by valio_stoyanov on 5/14/18.
//

#include "json_parser.h"
#include "../models/json_null.h"

JsonValue& JsonParser::parse(const String& json) {
    return JsonNull::instance();
}

bool JsonParser::is_json_object(const String& json) {
    const size_t json_length = json.length();
    for (int i = 0; i < json_length; ++i) {

    }
    return false;
}
