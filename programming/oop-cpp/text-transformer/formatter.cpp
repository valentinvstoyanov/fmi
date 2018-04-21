#include <cstring>
#include "formatter.h"

Formatter::Formatter(const char* delims/* = " \t"*/) {
    if (!delims)
        throw std::invalid_argument("nullptr passed as constructor argument to Formatter");
    delimeters = new char[strlen(delims) + 1];
    strcpy(delimeters, delims);
}

Formatter::Formatter(const Formatter& formatter) {
    delimeters = new char[strlen(formatter.delimeters) + 1];
    strcpy(delimeters, formatter.delimeters);
}

Formatter& Formatter::operator=(const Formatter& other) {
    if(this != &other) {
        delete[] delimeters;
        delimeters = new char[strlen(other.delimeters) + 1];
        strcpy(delimeters, other.delimeters);
    }
    return *this;
}

Formatter::~Formatter() {
    delete[] delimeters;
    delimeters = nullptr;
}

bool Formatter::is_delimeter(const char& c) const {
    for(unsigned i = 0; delimeters[i]; ++i)
        if(c == delimeters[i])
            return true;

    return false;
}

int Formatter::get_nth_word_index(const char* str, const unsigned n, const bool end/* = false*/) const {
    int i = 0;
    if(is_delimeter(str[i]) || str[i] == '#') {
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
        while(!is_delimeter(str[i]) && str[i]) {
            word_len++;
            i++;
        }
        word_counter++;
        if(word_counter == n)
            return (end ? i - 1 : i - word_len);
    }

    return -1;
}

char* Formatter::format(const char* str, const char* symbols, const bool bilateral) const {
    const unsigned str_length = strlen(str);
    const unsigned symbols_length = strlen(symbols);
    const unsigned result_length = str_length + (bilateral ? 2 * symbols_length : symbols_length);
    char* result_str = new char[result_length + 1];
    strcpy(result_str, symbols);
    strcat(result_str, str);
    if(bilateral)
        strcat(result_str, symbols);

    return result_str;
}

void Formatter::format(Line& line, const char* symbols, const bool bilateral, const unsigned from, const unsigned to) const {
    Line res = line;
    char* line_content = line.get_content();
    const unsigned from_word_index = get_nth_word_index(line_content, from);
    const unsigned to_word_index = get_nth_word_index(line_content, to, true);
    if(!(from_word_index < 0 || to_word_index < 0)) {
        const unsigned words_len = to_word_index - from_word_index + 1;
        char* words = new char[words_len + 1];
        words[words_len] = '\0';
        strncpy(words, line_content + from_word_index, words_len);
        char* formatted_words = format(words, symbols, bilateral);
        char* formatted_line_content = new char[line.get_length() - words_len + strlen(formatted_words) + 1];
        strncpy(formatted_line_content, line_content, from_word_index);
        formatted_line_content[from_word_index] = '\0';
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
}

Line Formatter::make_heading(const Line& line) const {
    Line formatted_line = line;
    format(formatted_line, "# ", false, 1, 2);
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

char* Formatter::change_filename_extension(const char* filename, const char* ext) const {
    const unsigned filename_len = strlen(filename);
    const unsigned ext_len = strlen(ext);
    int last_filename_index = get_nth_word_index(filename, 1, true);
    last_filename_index = (last_filename_index < 0 ? filename_len - 1 : last_filename_index);
    const unsigned res_len = last_filename_index + 2 + ext_len;
    char* res = new char[res_len + 1];
    res[res_len] = '\0';
    strncpy(res, filename, last_filename_index + 1);
    res[last_filename_index + 1] = '.';
    strcpy(res + last_filename_index + 2, ext);

    return res;
}
