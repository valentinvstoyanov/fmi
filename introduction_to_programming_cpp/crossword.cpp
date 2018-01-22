#include<iostream>

void destroy_dictionary(char** dictionary, const unsigned size) {
	if(dictionary) {
		for(unsigned i = 0; i < size; ++i)
			if(!dictionary[i])
				delete[] dictionary[i];
		delete[] dictionary;
	}
}

char** create_dictionary(const unsigned size) {
	char** dictionary = new(std::nothrow) char*[size];
	if(dictionary) {
		for(unsigned i = 0; i < size; ++i) {
			dictionary[i] = new(std::nothrow) char[40];
			if(!dictionary[i]) {
				destroy_dictionary(dictionary, size);
				return NULL;
			}
		}
	}
	
	return dictionary;
}

void read_words(char** dictionary, const unsigned words_count) {
	for(unsigned i = 0; i < words_count; ++i)
		std::cin >> dictionary[i];
}

void read_crossword(char* crossword, const unsigned rows_count, const unsigned cols_count) {
	for(unsigned i = 0; i < rows_count; ++i)
		for(unsigned j = 0; j < cols_count; ++j)
			std::cin >> crossword[i * cols_count + j];
}

void print_result(const bool is_valid) {
	std::cout << (is_valid ? "true" : "false") << std::endl;
}

unsigned max(const unsigned a, const unsigned b) {
	return a > b ? a : b;
}

bool is_valid_created_array(const void* arr) {
	if(arr) {
		return true;
	} else {
 		std::cout << "Failed to allocate dynamic memory." << std::endl;
		return false;
	}
}

bool is_letter(const char c) {
	return c >= 'a' && c <= 'z';
}

bool is_asterisk(const char c) {
	return c == '*';
}

bool are_words_equal(const char* w1, const char* w2) {
	for(unsigned i = 0; w1[i] && w2[i]; ++i)
		if(w1[i] != w2[i])
			return false;
	
	return true;
}

bool contains_word(char** dictionary, const unsigned dictionary_size, const char* word) {
	for(unsigned i = 0; i < dictionary_size; ++i)
		if(are_words_equal(dictionary[i], word))
			return true;

	return false;
}

bool validate_text(char** dictionary, const unsigned dictionary_size, const char* text, char* word) {
	unsigned i = 0;
	unsigned letters_counter = 0;

	while(text[i]) {
		if(is_letter(text[i])) {
			word[letters_counter] = text[i];
			letters_counter++;
		} else if(is_asterisk(text[i])) {
			if(letters_counter >= 2) {
				word[letters_counter] = '\0';
				if(!contains_word(dictionary, dictionary_size, word))
					return false;
			} else {
				letters_counter = 0;
			}
		}
		
		i++;
	}

	return true;
}

void check_crossword(char** dictionary, const unsigned dictionary_size, const char* crossword, const unsigned rows_count, const unsigned cols_count) {
	char* row_word = new(std::nothrow) char[cols_count + 1];
	if(is_valid_created_array(row_word)) {
		row_word[cols_count] = '\0';

		char* col_word = new(std::nothrow) char[rows_count + 1];
		if(is_valid_created_array(col_word)) {
			col_word[rows_count] = '\0';
			
			const unsigned word_holder_len = max(cols_count, rows_count);
			char* word_holder = new(std::nothrow) char[word_holder_len + 1];
			word_holder[word_holder_len] = '\0';
			if(is_valid_created_array(word_holder)) {
				word_holder[word_holder_len] = '\0';
		
				bool is_valid = true;

				for(unsigned i = 0; i < rows_count && is_valid; ++i) {
					for(unsigned j = 0; j < cols_count; ++j) {
						const char row_char = crossword[i * cols_count + j];
						const char col_char = crossword[j * rows_count + i];
						row_word[j] = row_char;
						col_word[i] = col_char;
					}
					is_valid &= validate_text(dictionary, dictionary_size, row_word, word_holder);
					is_valid &= validate_text(dictionary, dictionary_size, col_word, word_holder);
				}
				
				print_result(is_valid);

				delete[] word_holder;
			}	
			delete[] col_word;
		}
		delete[] row_word;
	}
}

int main() {
	unsigned words_count;
	std::cin >> words_count;
	char** dictionary = create_dictionary(words_count);

	if(is_valid_created_array(dictionary)) {
		read_words(dictionary, words_count);

		unsigned rows_count, cols_count;
		std::cin >> rows_count;
		std::cin >> cols_count;

		const unsigned crossword_len = rows_count * cols_count;
		char* crossword = new(std::nothrow) char[crossword_len + 1];
		if(is_valid_created_array(crossword)) {
			crossword[crossword_len] = '\0';
			read_crossword(crossword, rows_count, cols_count);
			check_crossword(dictionary, words_count, crossword, rows_count, cols_count);
			delete[] crossword;
		}

		destroy_dictionary(dictionary, words_count);
	}

	return 0;
}
