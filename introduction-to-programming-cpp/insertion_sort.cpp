#include<iostream>

void print_arr(const int arr[], const unsigned size) {
	for(unsigned i = 0; i < size; ++i)
		std::cout << arr[i] << " ";

	std::cout << std::endl;
}

void swap(int& a, int& b) {
	int temp = a;
	a = b;
	b = temp;
}

void insertion_sort(int arr[], const unsigned size) {
	for(unsigned i = 1; i < size; ++i)
		for(unsigned j = i; j > 0 && arr[j - 1] > arr[j]; --j)
			swap(arr[j - 1], arr[j]);
}

int main() {
	const unsigned size = 10;
	int arr[size] = {88, 100, 1, 2, 0, 60, 55, 0, 10, 222};
	insertion_sort(arr, size);
	print_arr(arr, size);

	return 0;
}
