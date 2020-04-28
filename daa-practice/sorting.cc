#include <iostream>

using namespace std;

const int MAX_SIZE = 1000;
unsigned arr[MAX_SIZE];
unsigned temp[MAX_SIZE];

void merge(unsigned* arr1, unsigned* arr2, int arr1_size, int arr2_size) {
	int i = 0;
	int j = 0;
	int c = 0;

	while (i < arr1_size && j < arr2_size) {
		if (arr1[i] <= arr2[j]) {
			temp[c++] = arr1[i];
			++i;
		} else {
			temp[c++] = arr2[j];
			++j;
		}
	}

	for (; i < arr1_size; ++i)
		temp[c++] = arr1[i];

	for (; j < arr2_size; ++j)
		temp[c++] = arr2[j];

	for (i = 0; i < arr1_size; ++i)
		arr1[i] = temp[i];
	for (j = 0; j < arr2_size; ++j)
		arr2[j] = temp[j + i];
	//for (i = 0; i < c; ++i)
	//	arr1[i] = temp[i];
}

void mergeSort(unsigned* arr, int low, int high) {
	if (low < high) {
		int middle = (low + high) / 2;
		mergeSort(arr, low, middle);
		mergeSort(arr, middle + 1, high);
		merge(arr + low, arr + middle + 1, middle - low + 1, high - middle);
	}
}

int main() {
	int n;
	scanf("%d", &n);

	for (int i = 0; i < n; ++i)
		scanf("%d", &arr[i]);

	mergeSort(arr, 0, n - 1);
	printf("%d", arr[0]);
	for (int i = 1; i < n; ++i)
		printf(" %d", arr[i]);

	printf("\n");

	return 0;
}
