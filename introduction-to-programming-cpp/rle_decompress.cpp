#include<iostream>

bool is_digit(const char);
unsigned append_digit_to_number(unsigned, unsigned short);
unsigned count_digits(const unsigned);
unsigned short char_to_digit(const char);
unsigned str_len(const char*);
char* str_concat(char*, const char*);
char* str_copy(char*, const char*);
char* str_resize(char*, unsigned);
char* str_repeat(char*, unsigned);
char* prepend_char_to_str(char*, const char);
unsigned extract_number(const char*);
unsigned count_group_length(const char*);
char* extract_group(const char*);
char* extract_decompressed_group(const char*);

//TODO: Fix dynamic memory issue and make it work.
char* rle_decompress(char* txt) {
	if(str_len(txt) <= 1)
		return txt;

	char c = *txt;
	
	if(is_digit(c)) {
		unsigned repeat_times = extract_number(txt);
		unsigned repeat_times_len = count_digits(repeat_times);
		char* decompressed_group = extract_decompressed_group(txt + repeat_times_len);
		unsigned decompressed_group_len = str_len(decompressed_group);
		char* repeated_decompressed_group = str_repeat(decompressed_group, repeat_times);
		char* rest_decompressed = rle_decompress(txt + (repeat_times_len + decompressed_group_len + 2));
	
		repeated_decompressed_group = str_resize(
							repeated_decompressed_group,
							(decompressed_group_len * repeat_times) + str_len(rest_decompressed)
		);

		repeated_decompressed_group = str_concat(repeated_decompressed_group, rest_decompressed);
		
		delete[] decompressed_group;
		delete[] rest_decompressed;
		return repeated_decompressed_group;
	} else {
		prepend_char_to_str(rle_decompress(txt + 1), c);
	}
}

int main() {
	char rle_compressed_str[] = "AAABBZ";
	char* rle_decompressed_str = rle_decompress(rle_compressed_str);
	if(rle_decompressed_str) {
		std::cout << rle_decompressed_str << std::endl;
		delete[] rle_decompressed_str;
	} else {
		std::cout << "Failed to allocate dynamic memory." << std::endl;
	}
	
	return 0;
}

bool is_digit(const char c) {
	return c >= '0' && c <= '9';
}

unsigned append_digit_to_number(unsigned num, unsigned short digit) {
	return num * 10 + digit;
}

unsigned count_digits(const unsigned num) {
	return num == 0 ? 0 : 1 + count_digits(num / 10);
}

unsigned short char_to_digit(const char c) {
	return c - '0';
}

unsigned str_len(const char* str) {
	unsigned len = 0;
	for(; str[len]; ++len);

	return len;
}

char* str_concat(char* dest, const char* src) {
	char* result = dest;

	while(*dest)
		dest++;

	for(unsigned i = 0; src[i]; ++i)
		*dest++ = src[i];

	*dest = '\0';
	
	return result;
}

char* str_copy(char* dest, const char* src) {
	char* result = dest;

	for(unsigned i = 0; src[i]; ++i)
		*dest++ = src[i];

	*dest = '\0';

	return result;
}

char* str_resize(char* str, unsigned new_length) {
	char* old_str = str;

	str = new(std::nothrow) char[new_length];
	if(!str) {
		delete[] old_str;
		return NULL;
	}

	str = str_copy(str, old_str);
	delete[] old_str;

	return str;
}

char* str_repeat(char* str, unsigned times) {
	char* repeated_str = new(std::nothrow) char[str_len(str) * times + 1];
	if(!repeated_str)
		return NULL;

	for(unsigned i = 0; i < times; ++i)
		repeated_str = str_concat(repeated_str, str);

	return repeated_str;
}

char* prepend_char_to_str(char* dest, const char c) {
	unsigned dest_len = str_len(dest);
	dest = str_resize(dest, dest_len + 2);
	if(!dest)
		return NULL;
	 
	for(unsigned i = dest_len; i > 0; --i)
		dest[i] = dest[i - 1];
	
	dest[0] = c;
	dest[dest_len + 1] = '\0';

	return dest;
}

unsigned extract_number(const char* str) {
	unsigned num = 0;

	while(is_digit(*str)) {
		num = append_digit_to_number(num, char_to_digit(*str));
		str++;
	}

	return num;
}

unsigned count_group_length(const char* str) {
	unsigned group_len = 0;
	unsigned parentheses_counter = 0;
	
	for(unsigned i = 0; str[i]; ++i) {
		char c = str[i];

		if(c == '(') {
			parentheses_counter++;
		} else if(c == ')') {
			parentheses_counter--;
			if(parentheses_counter == 0)
				break;
		}
	
		group_len++;
	}

	return group_len - 1;
}

char* extract_group(const char* str) {
	unsigned group_length = count_group_length(str);
	char* group = new(std::nothrow) char[group_length + 1];
	if(!group)
		return NULL;
	
	for(unsigned i = 0; str[i]; ++i) {
		if(str[i] == '(') {
			for(unsigned j = 0; j < group_length; ++j)
				group[j] = str[j + i + 1];
			break;
		}
	}

	group[group_length] = '\0';
	
	return group;
}


char* extract_decompressed_group(const char* str) {
	char* group = extract_group(str);
	if(!group)
		return NULL;

	char* decompressed_group = rle_decompress(group);
	delete[] group;
	
	return decompressed_group;
}
