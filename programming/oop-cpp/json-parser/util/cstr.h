//
// Created by valio_stoyanov on 6/26/18.
//

#ifndef JSON_PARSER_CSTR_H
#define JSON_PARSER_CSTR_H

#include <cstring>

int StrIndexOf(const char* str, char ch);
bool StrContains(const char* str, char ch);
const char* StrSkipWhiteSpace(const char* str);
unsigned StrCountCharLeadingOccurrence(const char* str, char ch);
bool IsDigit(char ch);
long long StrToLongLong(const char* str);


#endif //JSON_PARSER_CSTR_H
