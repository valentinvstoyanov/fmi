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



