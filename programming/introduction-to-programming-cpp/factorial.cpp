#include<iostream>

//Recursive factorial
unsigned recursive_factorial(const unsigned n) {
	return n == 0 ? 1 : n * recursive_factorial(n - 1);
}

//Iterative factorial
unsigned iterative_factorial(unsigned n) {
	unsigned result = 1;

	while(n > 0)
		result *= n--;

	return result;
}

int main() {
	const unsigned n1 = 5;
	std::cout << "recursive_factorial(" << n1 << ") = " << recursive_factorial(n1) << std::endl;

	const unsigned n2 = 6;
	std::cout << "iterative_factorial(" << n2 << ") = " << iterative_factorial(n2) << std::endl;

	return 0;
}
