//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_PAIR_H
#define JSON_PARSER_PAIR_H


#include "mystring.h"
#include "../models/json_value.h"

class Pair {
    String key_;
    JsonValue value_;
public:
    Pair();
    Pair(const String&, const JsonValue&);
    void set_value(const JsonValue&);
    void set_key(const String&);
    void set_pair(const String&, const JsonValue&);
    const String& get_key() const;
    const JsonValue& get_value() const;
};


#endif //JSON_PARSER_PAIR_H
