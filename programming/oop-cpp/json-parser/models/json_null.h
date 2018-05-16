//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_NULL_H
#define JSON_PARSER_JSON_NULL_H


#include "json_value.h"

class JsonNull: public JsonValue {
    JsonNull() = default;
public:
    JsonNull(const JsonNull&) = delete;
    JsonNull& operator=(const JsonNull&) = delete;
    static JsonNull& instance() const;
};


#endif //JSON_PARSER_JSON_NULL_H
