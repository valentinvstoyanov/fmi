#include <iostream>

using namespace std;

const int MAX_SIZE = 100000;
unsigned long long arr[MAX_SIZE];
unsigned long long temp[MAX_SIZE];

void merge(unsigned long long* arr1, unsigned long long* arr2, int arr1_size, int arr2_size) {
	int i = 0;
	int j = 0;
	int c = 0;

	while (i < arr1_size && j < arr2_size) {
		if (arr1[i] <= arr2[j]) {
			temp[c++] = arr1[i];
			++i;
		}
		else {
			temp[c++] = arr2[j];
			++j;
		}
	}

	for (int c1 = i; i < arr1_size; ++i)
		temp[c++] = arr1[c1];

	for (int c2 = j; j < arr2_size; ++j)
		temp[c++] = arr2[c2];

	for (int c3 = 0; c3 < c; ++c3)
		arr1[c3] = temp[c3];
}

void mergeSort(unsigned long long* arr, int low, int high) {
	if (low < high) {
		int middle = (low + high) / 2;
		mergeSort(arr, low, middle);
		mergeSort(arr, middle + 1, high);
		merge(arr + low, arr + middle + 1, middle - low + 1, high - middle);
	}
}

int main() {
	int n;
	cin >> n;

	for (int i = 0; i < n; ++i)
		cin >> arr[i];

	mergeSort(arr, 0, n - 1);

	for (int i = 0; i < n; ++i)
		cout << arr[i] << (i == n - 1 ? "" : " ");

	return 0;
}
