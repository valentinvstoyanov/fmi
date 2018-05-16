//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_PARSER_H
#define JSON_PARSER_JSON_PARSER_H


#include "../models/json_value.h"
#include "../ds/mystring.h"

class JsonParser {
    bool is_json_object(const String&);
    bool is_json_number(const String&);
    bool is_json_boolean(const String&);
    bool is_json_null(const String&);
    bool is_json_array(const String&);
    bool is_json_string(const String&);
public:
    JsonValue& parse(const String&);
};


#endif //JSON_PARSER_JSON_PARSER_H
