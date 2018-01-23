#include<iostream>

//Classic recursive fibonacci
unsigned fibonacci(const unsigned n) {
	switch(n) {
		case 0:
			return 0;
		case 1:
			return 1;
		default:
			return fibonacci(n - 1) + fibonacci(n - 2);
	}
}


//Optimized recursive fibonacci
unsigned optimized_fibonacci(const unsigned n, const unsigned previous, const unsigned accumulator) {
	if(n == 0)
		return accumulator;

	return optimized_fibonacci(n - 1, accumulator, previous + accumulator);
}
		
unsigned optimized_fibonacci_wrapper(const unsigned n) {
	return optimized_fibonacci(n, 1, 0);
}

//Iterative fibonacci
unsigned iterative_fibonacci(unsigned n) {
	unsigned accumulator = 0;
	unsigned previous = 1;
	
	while(n > 0) {
		unsigned temp = previous;
		previous = accumulator;
		accumulator += temp;
		n--;
	}

	return accumulator;
}

int main() {
	const unsigned n1 = 29;
	std::cout << "fibonacci(" << n1 << ") = " << fibonacci(n1) << std::endl;

	const unsigned n2 = 100;
	std::cout << "optimized_fibonacci_wrapper(" << n2 << ") = " << optimized_fibonacci_wrapper(n2) << std::endl;
	
	const unsigned n3 = 24;
	std::cout << "iterative_fibonacci(" << n3 << ") = " << iterative_fibonacci(n3) << std::endl;

	return 0;
}
