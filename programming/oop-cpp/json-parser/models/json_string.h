//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_STRING_H
#define JSON_PARSER_JSON_STRING_H


#include "../ds/mystring.h"
#include "json_value.h"

class JsonString: public JsonValue {

    //TODO: maybe it has to inherit from String

    String string_;
public:
    explicit JsonString(const char*);
    explicit JsonString(const String&);
    JsonString(const JsonString&);
    ~JsonString();
    JsonString& operator=(const JsonString&);
};


#endif //JSON_PARSER_JSON_STRING_H
