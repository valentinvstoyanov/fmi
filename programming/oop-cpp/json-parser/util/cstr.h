#ifndef JSON_PARSER_CSTR_H
#define JSON_PARSER_CSTR_H

#include <cstring>

int StrIndexOf(const char* str, char ch);
bool StrContains(const char* str, char ch);
const char* StrSkipWhiteSpace(const char* str);
unsigned StrCountCharLeadingOccurrence(const char* str, char ch);
bool IsDigit(char ch);


#endif //JSON_PARSER_CSTR_H
