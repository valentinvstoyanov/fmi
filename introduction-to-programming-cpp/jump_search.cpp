#include<iostream>
#include<cmath>

int linear_search(const int arr[], const unsigned begin_index, const unsigned end_index, const int element) {
	for(unsigned i = begin_index; i <= end_index; ++i)
		if(arr[i] == element)
			return i;

	return -1;
}

int jump_search(const int arr[], const unsigned size, const int element) {
	const unsigned block_size = sqrt(size);
	unsigned begin_index = 0;

	for(unsigned end_index = block_size; end_index < size; end_index += block_size) {
		if(arr[begin_index] <= element && arr[end_index] >= element)
			return linear_search(arr, begin_index, end_index, element);
	
		begin_index = end_index;
	}

	return -1;
}

int main() {
	const unsigned size = 10;
	const int arr[size] = {1, 2, 3, 4, 5, 55, 60, 100, 555, 999};
	const int search_element = 2;
	const int search_result = jump_search(arr, size, search_element);

	std::cout << "Result: " << search_result << std::endl;

	return 0;
}
