#include <cstring>
#include "formatter.h"

Formatter::Formatter() {}

char* Formatter::format(const char* str, const char* symbols, const bool bilateral) {
    const unsigned str_length = strlen(str);
    const unsigned symbols_length = strlen(symbols);
    const unsigned result_length = str_length + (bilateral ? symbols_length + 1 : symbols_length + 2);
    char* result_str = new char[result_length + 1];
    strcat(result_str, symbols);
    strcat(result_str, " ");
    strcat(result_str, str);
    if(bilateral) {
        strcat(result_str, " ");
        strcat(result_str, symbols);
    }
    return result_str;
}
