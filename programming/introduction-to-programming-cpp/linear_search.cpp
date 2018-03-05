#include<iostream>

void print_array(const int arr[], const unsigned size) {
	std::cout << "{ ";	
	for(unsigned i = 0; i < size; ++i)
		std::cout << arr[i] << " ";

	std::cout << "}" << std::endl;
}

int linear_search(const int arr[], const unsigned size, const int element) {
	for(unsigned i = 0; i < size; ++i)
		if(arr[i] == element)
			return i;

	return -1;
}

int main() {
	const unsigned size = 7;
	const int arr[size] = {5, 4, 20, 3, 2, 100, -1};
	const int search_element = 3;
	const int search_result = linear_search(arr, size, search_element);

	std::cout << "Searched for element: " << search_element << " in ";
	print_array(arr, size);

	if(search_result >= 0)
		std::cout << "Found at position: " << search_result;
	else
		std::cout << "Not found!";

	std::cout << std::endl;

	return 0;
}
