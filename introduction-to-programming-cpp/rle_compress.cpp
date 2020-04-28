#include <iostream>
#include <string>

int main() {
	std::string input;

	std::cout << "Please enter some string: ";
	std::cin >> input;

	for(int i = 0; i < input.length();) {
		int j = i + 1;
		while(input[i] == input[j] && j < input.length()) 
			j++;
		
		std::cout << (j - i) << input[i];
		i = j;
	}

	std::cout << std::endl;

	return 0;
}
