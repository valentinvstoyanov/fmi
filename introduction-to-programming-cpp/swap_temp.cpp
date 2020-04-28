#include<iostream>

int main() {
	int a, b;
	std::cin >> a;
	std::cin >> b;

	int temp = a;
	a = b;
	b = temp;

	std::cout << "Swapped values: " << a << " " << b << std::endl;

	return 0;
}
