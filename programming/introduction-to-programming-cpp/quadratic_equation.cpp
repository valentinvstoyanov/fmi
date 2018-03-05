#include<iostream>	
#include<cmath>

int read_number(std::string print_text) {
	double number;
	std::cout << print_text;
	std::cin >> number;

	return number;
}

double solve_linear_equation(double b, double c) {
	return c == 0.0 ? 0 : ((-c) / b);
}

double calculate_discriminant(double a, double b, double c) {
	return (b * b) - (4 * a * c);
}

double calculate_root(double b, double d, double a, short sign) {
	return ((-b) + (sign * std::sqrt(d))) / (2 * a);
}

int main() {
	double a = read_number("a = ");
	double b = read_number("b = ");
	double c = read_number("c = ");

	if(a == 0.0) {
		(b == 0.0) ? (std::cout << "No solution!" << std::endl) : (std::cout << "x = " << solve_linear_equation(b, c) << std::endl);
	} else {
		double d = calculate_discriminant(a, b, c);
		(d < 0) ? (std::cout << "No solution" << std::endl) : (std::cout << "x1 = " << calculate_root(b, d, a, 1) << ", x2 = " << calculate_root(b, d, a, -1) << std::endl);
	}
	
	return 0;
}
