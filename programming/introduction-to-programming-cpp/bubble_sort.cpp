#include<iostream>

void swap(int& a, int& b) {
	int temp = a;
	a = b;
	b = temp;
}

void print_arr(const int arr[], const unsigned size) {
	for(unsigned i = 0; i < size; ++i)
		std::cout << arr[i] << " ";

	std::cout << std::endl;
}


void bubble_sort(int arr[], const unsigned size) {
	bool swapped = true;
	
	for(unsigned i = 0; i < size - 1 && swapped; ++i) {
		swapped = false;

		for(unsigned j = 0; j < size - i - 1; ++j) {
			if(arr[j] > arr[j + 1]) {
				swap(arr[j], arr[j + 1]);
				swapped = true;
			}
		}
	}
}

int main() {
	const unsigned size = 10;
	int arr[size] = {10, 900, 8, 7, 4, 5, 6, 2, 1, 3};
	bubble_sort(arr, size);
	print_arr(arr, size);

	return 0;
}
