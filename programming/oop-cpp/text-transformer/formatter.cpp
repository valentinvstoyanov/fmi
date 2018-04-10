#include "formatter.h"
#include <cstring>

Formatter::Formatter(const char* delims/* = " \t"*/) {
    if (delims) {
        delimeters = new char[strlen(delims) + 1];
        strcpy(delimeters, delims);
    } else {
        throw std::invalid_argument("nullptr passed as constructor argument to Formatter");
    }
}

Formatter::~Formatter() {
    delete[] delimeters;
    delimeters = nullptr;
}

char* Formatter::format(const char* str, const char* symbols, const bool bilateral) const {
    const unsigned str_length = strlen(str);
    const unsigned symbols_length = strlen(symbols);
    const unsigned result_length = str_length + (bilateral ? 2 * symbols_length + 2 : symbols_length + 1);
    char* result_str = new char[result_length + 1];
    strcpy(result_str, symbols);
    strcat(result_str, " ");
    strcat(result_str, str);
    if(bilateral) {
        strcat(result_str, " ");
        strcat(result_str, symbols);
    }
    return result_str;
}

bool Formatter::is_delimeter(const char& c) const {
    for(unsigned i = 0; delimeters[i]; ++i)
        if(c == delimeters[i])
            return true;

    return false;
}

int Formatter::get_nth_word_end_index(const char* str, const unsigned n) const {
    int i = 0;
    if(is_delimeter(str[i])) {
        i++;
        while(is_delimeter(str[i]))
            i++;
    }
    unsigned word_len = 0;
    unsigned word_counter = 0;
    while(str[i]) {
        word_len = 0;
        while(is_delimeter(str[i]))
            i++;
        while(!is_delimeter(str[i])) {
            word_len++;
            i++;
        }
        word_counter++;
        if(word_counter == n) {
            return i - 1;
        }
    }

    return -1;
}

int Formatter::get_nth_word_start_index(const char* str, const unsigned n) const {
    int i = 0;
    if(is_delimeter(str[i])) {
        i++;
        while(is_delimeter(str[i]))
            i++;
    }
    unsigned word_len = 0;
    unsigned word_counter = 0;
    while(str[i]) {
        word_len = 0;
        while(is_delimeter(str[i]))
            i++;
        while(!is_delimeter(str[i])) {
            word_len++;
            i++;
        }
        word_counter++;
        if(word_counter == n) {
            return i - word_len;
        }
    }

    return -1;
}

void Formatter::format(Line& line, const char* symbols, const bool bilateral, const unsigned from, const unsigned to) const {
    Line res = line;
    char* line_content = line.get_content();
    const unsigned from_word_index = get_nth_word_start_index(line_content, from);
    const unsigned to_word_index = get_nth_word_end_index(line_content, to);
    if(!(from_word_index < 0 || to_word_index < 0)) {
        const unsigned words_len = to_word_index - from_word_index + 1;
        char* words = new char[words_len + 1];
        strncpy(words, line_content + from_word_index, words_len);
        char* formatted_words = format(words, symbols, bilateral);
        char* formatted_line_content = new char[line.get_length() - words_len + strlen(formatted_words) + 1];
        formatted_line_content[0] = '\0';
        strncpy(formatted_line_content, line_content, from_word_index);
        strcat(formatted_line_content, formatted_words);
        strcat(formatted_line_content, line_content + from_word_index + words_len);
        res.set_content(formatted_line_content);
        delete[] formatted_line_content;
        formatted_line_content = nullptr;
        delete[] formatted_words;
        formatted_words = nullptr;
        delete[] words;
        words = nullptr;
    }
    delete[] line_content;
    line_content = nullptr;
    line = res;
    //return line;
}

Line Formatter::make_heading(const Line& line) const {
    Line formatted_line = line;
    format(formatted_line, "#", false, 1, 2);
    return formatted_line;
}

Line Formatter::make_bold(const Line& line, const unsigned from, const unsigned to) const {
    Line formatted_line = line;
    format(formatted_line, "**", true, from, to);
    return formatted_line;
}

Line Formatter::make_italic(const Line& line, const unsigned from, const unsigned to) const {
    Line formatted_line = line;
    format(formatted_line, "*", true, from, to);
    return formatted_line;
}

Line Formatter::make_combined(const Line& line, const unsigned from, const unsigned to) const {
    Line formatted_line = line;
    format(formatted_line, "***", true, from, to);
    return formatted_line;
}
