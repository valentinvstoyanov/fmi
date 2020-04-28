#include <iostream>
#include <algorithm>

const unsigned long long MAX_N = 100000;

unsigned long long arr[MAX_N];

int main() {
	unsigned long long N, K;
	unsigned long long M;
	std::cin >> N >> M >> K;

	unsigned long long previous;
	unsigned long long current;
	std::cin >> previous;

	for (unsigned long long i = 0; i < N - 1; ++i) {
		std::cin >> current;
		arr[i] = current - previous - 1;
		previous = current;	
	}

	std::sort(arr, arr + (N - 1));

	unsigned long long sum = N;
	for (unsigned long long i = 0; i < N - K; ++i)
		sum += arr[i];

	std::cout << sum << std::endl;

	return 0;
}
