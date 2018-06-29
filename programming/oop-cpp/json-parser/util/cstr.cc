//
// Created by valio_stoyanov on 6/26/18.
//

#include <stdexcept>
#include "cstr.h"

static const char* kStrWhitespace = " \n\r\t\v\f";

int StrIndexOf(const char* str, char ch) {
  for (int i = 0; str[i]; ++i)
    if (str[i] == ch)
      return i;

  return -1;
}

bool StrContains(const char* str, char ch) {
  return StrIndexOf(str, ch) >= 0;
}

const char* StrSkipWhiteSpace(const char* str) {
  while (*str && StrContains(kStrWhitespace, *str)) str++;
  return str;
}

unsigned StrCountCharLeadingOccurrence(const char* str, char ch) {
  unsigned counter = 0;
  while (*str && *str == ch) {
    ++str;
    ++counter;
  }

  return counter;
}

bool IsDigit(char ch) {
  return ch >= '0' && ch <= '9';
}

long long StrToLongLong(const char* str) {
  if (!str) throw std::invalid_argument("Cannot parse number from null pointer.");
  if (!*str) throw std::invalid_argument("Cannot parse number from empty string.");

  long long result = 1LL;

  const unsigned neg_sign_occur = StrCountCharLeadingOccurrence(str, '-');
  if (neg_sign_occur > 1)
    throw std::invalid_argument("Cannot parse number. It has too many negative signs.");
  else if (neg_sign_occur == 1)
    result *= -1LL;
  else
    if (StrCountCharLeadingOccurrence(str, '+') > 1)
      throw std::invalid_argument("Cannot parse number. It has too many positive signs.");

  ++str;

  while (*str) {
    if (!IsDigit(*str))
      throw std::invalid_argument("Cannot parse number. Non-digit character found.");

  }

  return result;
}