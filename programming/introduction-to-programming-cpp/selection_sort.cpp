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

int find_min_index(const int arr[], const unsigned from, const unsigned to) {
	int min_index = from;

	for(unsigned i = from + 1; i <= to; ++i)
		if(arr[i] < arr[min_index])
			min_index = i;

	return min_index;
}

void selection_sort(int arr[], const unsigned size) {
	for(unsigned i = 0; i < size; ++i) {
		int min_index = find_min_index(arr, i, size - 1);
		swap(arr[i], arr[min_index]);
	}
}

int main() {
	const int size = 10;
	int arr[size] = {1000, 1, 101, 1001, 2, 3, 10, 5, 6, 1000};
	selection_sort(arr, size);
	print_arr(arr, size);

	return 0;
}
