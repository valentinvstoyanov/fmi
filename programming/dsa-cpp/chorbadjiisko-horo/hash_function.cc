//
// Created by valio_stoyanov on 12/12/18.
//

#include <cmath>
#include "hash_function.h"

size_t HashFunction::operator()(const std::string& str) const {
  size_t result = 0;
  for (size_t i = 0; i < str.size(); ++i)
    result += std::pow(10, str.size() - (i + 1)) * str[i];
  return result;
}
