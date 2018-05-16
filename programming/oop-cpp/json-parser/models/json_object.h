//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_JSON_OBJECT_H
#define JSON_PARSER_JSON_OBJECT_H


#include "json_value.h"
#include "../ds/pair.h"

class JsonObject: public JsonValue {
    Pair* pairs_;
    size_t size_;
    size_t capacity_;
    void ensure_capacity(const size_t);
public:
    explicit JsonObject(size_t = 1);
    JsonObject(const JsonObject&);
    ~JsonObject();
    JsonObject& operator=(const JsonObject&);
    void push_back(const Pair&);
    void remove(const size_t);
    int index_of(const String&) const;
    void remove(const String&);
    bool exists(const String&) const;
    size_t size() const;
    bool empty() const;
};


#endif //JSON_PARSER_JSON_OBJECT_H
