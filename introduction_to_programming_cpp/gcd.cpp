#include<iostream>

int gcd(const int a, const int b) {
	return b == 0 ? a : gcd(b, a % b);
}

int main() {
	const int a = 729;
	const int b = 81;

	std::cout << "gcd(" << a << ", " << b << ") = " << gcd(a, b) << std::endl;

	return 0;
}
