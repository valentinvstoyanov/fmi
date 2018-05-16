//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_ARRAY_H
#define JSON_PARSER_JSON_ARRAY_H


#include <glob.h>
#include "json_value.h"

class JsonArray: public JsonValue {
    JsonValue** values_;
    size_t size_;
    size_t capacity_;
    void ensure_capacity(size_t);
public:
    explicit JsonArray(size_t = 1);
    JsonArray(const JsonArray&);
    ~JsonArray();
    JsonArray& operator=(const JsonArray&);
    /*void push_back(JsonValue*);
    void append(const JsonArray&);
    void remove(const size_t);
    JsonValue& at(const size_t);
    const JsonValue& at(const size_t) const;*/
    bool empty() const;
    size_t size() const;
};


#endif //JSON_PARSER_JSON_ARRAY_H
