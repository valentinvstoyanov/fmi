#include<iostream>
#include<cstdlib>
#include<ctime>

unsigned get_random_index(const unsigned from, const unsigned to) {
	return rand() % to + from;
}

unsigned get_word_length(const char* word) {
	unsigned length = 0;
	while(word[length])
		length++;

	return length;
}

bool reveal_letters(const char* word, char* hidden_word, const char letter) {
	bool change = false;
	for(unsigned i = 0; word[i]; ++i) {
		if(word[i] == letter && hidden_word[i] != letter) {
			change = true;
			hidden_word[i] = letter;
		}
	}

	return change;
}

bool is_word_guessed(const char* hidden_word) {
	for(int i = 0; hidden_word[i]; ++i)
		if(hidden_word[i] == '_')
			return false;

	return true;
}

void print_separated(const char* text) {
	for(unsigned i = 0; text[i]; ++i)
		std::cout << text[i] << ' ';
}

void print_info(const unsigned short mistakes, const unsigned short max_mistakes, const char* word, const char* history) {
	std::cout << '[' << mistakes << '/' << max_mistakes << "] ";
	print_separated(word);
	std::cout << "History: ";
	print_separated(history);
	std::cout << std::endl;
}

int main() {
	srand(time(NULL));
	const char* words[] = {"apple", "orange", "month", "vehicle"};
	const unsigned short words_count = 4;
	const unsigned short max_mistakes = 5;
	const char* word = words[get_random_index(0, words_count)];
	const unsigned short word_length = get_word_length(word);
	char* hidden_word = new(std::nothrow) char[word_length + 1];
	char* history = new(std::nothrow) char[word_length + max_mistakes + 1];
	unsigned short history_length = 0;

	if(hidden_word && history) {
		for(unsigned i = 0; i < word_length; ++i)
			hidden_word[i] = '_';
		hidden_word[word_length] = '\0';
		history[history_length] = '\0';

		unsigned short mistakes_count = 0;
		bool guessed = false;
		std::cout << "Guess the word (max " << max_mistakes << " mistakes):" << std::endl;

		while(!guessed && mistakes_count <= max_mistakes) {
			print_info(mistakes_count, max_mistakes, hidden_word, history);

			char letter;
			std:: cout << "Choose a letter: ";
			std::cin >> letter;

			history[history_length] = letter;
			history[++history_length] = '\0';

			if(reveal_letters(word, hidden_word, letter)) {
				std::cout << "OK" << std::endl;
				guessed = is_word_guessed(hidden_word);
			} else {
				mistakes_count++;
				std::cout << "No such letter!" << std::endl;
			}
		}
			
		if(guessed)
			std::cout << "Congratulations, you guessed the word: " << word << " !" << std::endl;
		else 
			std::cout << "Game over, please try again." << std::endl;

		delete[] hidden_word;
		delete[] history;
	} else {
		std::cout << "Failed to allocate dynamic memory" << std::endl;
	}

	return 0;
}
