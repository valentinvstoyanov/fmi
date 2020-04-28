#include<iostream>
#include<cstring>

using namespace std;

const unsigned BUFF_SIZE = 256;

bool equals(const char* str1, const unsigned len1, const char* str2, const
unsigned len2) {
    if(*str1 != *str2 || len1 != len2)
	return false;

    if(len1 == 1)
	return true;

    return equals(str1 + 1, len1 - 1, str2 + 1, len2 - 1);
}

unsigned count_occurrences(const char* text, const unsigned text_len, const
char* word, const unsigned word_len) {
    if(text_len < word_len)
	return 0;

    if(equals(text, word_len, word, word_len))
	return 1 + count_occurrences(text + word_len, text_len - word_len,
	word, word_len);
    else
	return count_occurrences(text + 1, text_len - 1, word, word_len);
}

unsigned calculate_new_str_len(const unsigned old_len, const unsigned
old_word_len, const unsigned word_occur, const unsigned new_word_len) {
    return old_len + word_occur * (new_word_len - old_word_len);
}

char* replace_in_str(char* text, char* what, char* with) {
    const unsigned text_len = strlen(text);
    const unsigned what_len = strlen(what);
    const unsigned with_len = strlen(with);
    const unsigned word_occur = count_occurrences(text, text_len, what,
    what_len);
    const unsigned new_str_len = calculate_new_str_len(text_len, what_len,
    word_occur, with_len);
    char* new_str = new(nothrow) char[new_str_len + 1];
    if(!new_str)
	return NULL;
    new_str[new_str_len] = '\0';

}

int main() {

    char text[BUFF_SIZE];
    char what[BUFF_SIZE];
    char with[BUFF_SIZE];
    
    cout << "Text:";
    cin.getline(text, BUFF_SIZE);

    cout << "What: ";
    cin.getline(what, BUFF_SIZE);

    cout << "With: ";
    cin.getline(with, BUFF_SIZE);
   cout << count_occurrences("kurzavaszanaz", 13, "za", 2) << endl; 
    char* result = replace_in_str(text, what, with);
    if(!result) {
	cerr << "Failed to allocate dynamic memory" << endl;
	return 0;
    }

    cout << "Result: " << result << endl;
    delete[] result;
    
    return 0;
}
