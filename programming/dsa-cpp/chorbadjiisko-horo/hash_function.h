//
// Created by valio_stoyanov on 12/12/18.
//

#ifndef CHORBADJIISKO_HORO_HASH_FUNCTION_H
#define CHORBADJIISKO_HORO_HASH_FUNCTION_H

#include <string>

struct HashFunction {
  size_t operator()(const std::string&) const;
};

#endif //CHORBADJIISKO_HORO_HASH_FUNCTION_H
