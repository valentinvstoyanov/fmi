#include<iostream>
#include<cstdlib>
#include<ctime>

unsigned get_random_number(const unsigned from, const unsigned to) {
	return (rand() % to) + from;
}

unsigned get_text_length(const char* text) {
	unsigned length = 0;
	while(text[length])
		length++;

	return length;
}

int find_first_valid_after_index(const bool* possible_options, unsigned max_index, const unsigned index) {
	for(unsigned i = index; i <= max_index; ++i)
		if(possible_options[i])
			return i;

	for(unsigned i = 0; i <= index; i++)
		if(possible_options[i])
			return i;
	
	return -1;
}

unsigned get_object_subject_index(const bool* possible_options, const unsigned max_index) {
	unsigned random_index = get_random_number(0, max_index + 1);

	if(possible_options[random_index]) {
		return random_index;
	} else {
		return find_first_valid_after_index(possible_options, max_index, get_random_number(0, max_index + 1));
	}
}

char* concatenate(char* dest_ptr, const char delimeter, const char* src_ptr) {
	if(!(dest_ptr && src_ptr))	
		return NULL;

	char* res_ptr = dest_ptr;

	while(*dest_ptr)
		dest_ptr++;

	if(delimeter)
		*dest_ptr++ = delimeter;

	while(*src_ptr) {
		*dest_ptr = *src_ptr;
		dest_ptr++;
		src_ptr++;
	}
	*dest_ptr = '\0';

	return res_ptr;
}

char* generate_heading(const char* exclamation, const char* object, const char* action, const char* subject) {
	const unsigned exclamation_length = get_text_length(exclamation);
	const unsigned exclamation_marks_count = get_random_number(3, 10);
	const unsigned object_length = get_text_length(object);
	const unsigned action_length = get_text_length(subject);
	const unsigned subject_length = get_text_length(subject);
	const unsigned heading_length = exclamation_length + exclamation_marks_count + object_length + action_length + subject_length + 3 + 1;
	
	char* heading = new(std::nothrow) char[heading_length];
	if(!heading)
		return NULL;

	char* exclamation_marks = new(std::nothrow) char[exclamation_marks_count + 1];
	if(!exclamation_marks)
		return NULL;
	for(unsigned i = 0; i < exclamation_marks_count; ++i)
		exclamation_marks[i] = '!';
	exclamation_marks[exclamation_marks_count] = '\0';

	concatenate(heading, '\0', exclamation);
	concatenate(heading, '\0', exclamation_marks);
	delete[] exclamation_marks;
	concatenate(heading, ' ', object);
	concatenate(heading, ' ', action);
	concatenate(heading, ' ', subject);

	return heading;
}

//TODO: Fix random number generation
int main() {
	srand(time(NULL));
	const char* const exclamations[] = {
		"SHOK",
		"Skandal",
		"Nechuvana naglost"
	};

	const char* const actions[] = {
		"sgazi",
		"zadiga",
		"namiga na",
		"precakva",
		"tarashi"
	};

	const char* const objects_and_subjects[] = {
		"Riapa",
		"Baba",
		"Kmet",
		"Bager",
		"Sklad"
	};
	
	const bool object_possibilities[] = {
		true, true, true, true, true
	};

	const bool subject_possibilities[] = {
		true, true, true, true, false
	};

	unsigned headings_count;
	std::cout << "Enter number of headings: ";
	std::cin >> headings_count;

	for(unsigned i = 0; i < headings_count; ++i) {
		char* heading = generate_heading(
							exclamations[get_random_number(0, 3)],
							objects_and_subjects[get_object_subject_index(object_possibilities, 4)],
							actions[get_random_number(0, 5)],
							objects_and_subjects[get_object_subject_index(subject_possibilities, 4)]
		);

		if(heading) {
			std::cout << heading << std::endl;
			delete[] heading;
		} else {
			std::cout << "Failed to allocate dynamic memory." << std::endl;
		}
	}

	return 0;
}
