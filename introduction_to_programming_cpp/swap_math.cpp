#include<iostream>

int main() {
	int a, b;
	std::cin >> a;
	std::cin >> b;

	a += b;
	b = a - b;
	a -= b;

	std::cout << "Swapped values: " << a << " " << b << std::endl; 
	
	return 0;
}
