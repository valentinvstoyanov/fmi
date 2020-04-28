#include<iostream>

void print_array(const int arr[], const unsigned size) {
	std::cout << "{ ";	
	for(unsigned i = 0; i < size; ++i)
		std::cout << arr[i] << " ";

	std::cout << "}" << std::endl;
}

//Iterative binary search
int iterative_binary_search(const int arr[], const unsigned size, const int element) {
	unsigned left = 0;
	unsigned right = size - 1;

	while(left <= right) {
		unsigned middle = (left + right) / 2;

		if(arr[middle] == element)
			return middle;

		if(element < arr[middle])
			right = middle - 1;
		else
			left = middle + 1;
	}

	return -1;
}

//Recursive binary search
int recursive_binary_search(const int arr[], const unsigned left, const unsigned right, const int element) {
	if(left > right)
		return -1;

	unsigned middle = (left + right) / 2;
	
	if(arr[middle] == element)
		return middle;

	if(element < arr[middle])
		return recursive_binary_search(arr, left, middle - 1, element);
	else
		return recursive_binary_search(arr, middle + 1, right, element);
}

int recursive_binary_search_wrapper(const int arr[], const unsigned size, const int element) {
	return recursive_binary_search(arr, 0, size - 1, element);
}

int main() {
	const unsigned size = 10;
	const int arr[size] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	const int search_element = 5;
	const int iterative_search_result = iterative_binary_search(arr, size, search_element);
	
	std::cout << "Searched for element: " << search_element << " in ";
	print_array(arr, size);

	if(iterative_search_result >= 0)
		std::cout << "Found at position: " << iterative_search_result;
	else
		std::cout << "Not found!";

	std::cout << std::endl;

	const int recursive_binary_search = recursive_binary_search_wrapper(arr, size, search_element);
	std::cout << "Recursive approach result: " << recursive_binary_search << std::endl;

	return 0;
}
