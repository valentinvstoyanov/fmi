#include<iostream>

int main() {
	const unsigned short MAX_SQUARE_SIZE = 10;
	int column_sums[MAX_SQUARE_SIZE];
	unsigned short square_size;
	std::cout << "Enter N <= 10: ";
	std::cin >> square_size;
	bool is_magic = true;
	
	int first_diagonal_sum = 0;
	int second_diagonal_sum = 0;
	int previous_row_sum = 0;
	for(unsigned i = 0; i < square_size; ++i) {
		int current_row_sum = 0;
		for(unsigned j = 0; j < square_size; ++j) {
			int num;
			std::cin >> num;
			current_row_sum += num;
			column_sums[j] += num;
			if(i == j)
				first_diagonal_sum += num;
			if(j == square_size - i - 1)
				second_diagonal_sum += num;
		}
		if(i == 0)
			previous_row_sum = current_row_sum;
		is_magic &= (previous_row_sum == current_row_sum);
		previous_row_sum = current_row_sum;
	}

	if(!is_magic) {
		is_magic &= (first_diagonal_sum == second_diagonal_sum) && (first_diagonal_sum == previous_row_sum);

		for(unsigned i = 0; i < square_size; ++i)
			is_magic &= (column_sums[i] == first_diagonal_sum);
	}

	std::cout << (is_magic ? "True" : "False") << std::endl;
	return 0;
}
