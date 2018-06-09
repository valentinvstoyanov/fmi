//
// Created by valio_stoyanov on 5/14/18.
//

#ifndef JSON_PARSER_PAIR_H
#define JSON_PARSER_PAIR_H

#include "mystring.h"

template<typename K, typename V>
class Pair {
  K key_;
  V value_;
 public:
  Pair(const K& key, const V& value)
      : key_(key), value_(value) {}

  void SetKey(const K& key) {
    key_ = key;
  }

  void SetValue(const V& value) {
    value_ = value;
  }

  void Set(const K& key, const V& value) {
    key_ = key;
    value_ = value;
  }

  K& Key() {
    return key_;
  }

  V& Value() {
    return value_;
  }

  const K& Key() const {
    return key_;
  }

  const V& Value() const {
    return value_;
  }
};

#endif //JSON_PARSER_PAIR_H
