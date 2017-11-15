#include<iostream>

int main() {
	int sequence_length;
	std::cin >> sequence_length;

	int previous;
	int current;
	int next;
	bool result = true;

	for(int i = 0; i < sequence_length; i++) {
		if(i == 0) {
			std::cin >> current;
			previous = current;
			i++;
		}

		if(sequence_length > 1) std::cin >> next;

		result &= (current <= previous && current <= next) || (current >= previous && current >= next);
		previous = current;
		current = next;
	}

	std::cout << (result ? "yes" : "no") << std::endl;

	return 0;
}
