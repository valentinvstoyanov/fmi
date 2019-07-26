#include<iostream>
using namespace std;

const int MAX_SIZE = 1001;

int n;
int arr[MAX_SIZE];

int s() {
	int counter = 0;
	for (int i = 2; i <= n; ++i) {
		int key = arr[i];
		int j = i - 1;
		while (j > 0 && arr[j] > key) {
			arr[j + 1] = arr[j];
			--j;
			++counter;
		}
		arr[j + 1] = key;	
	}
	return counter;
}

int main() {
	scanf("%d", &n);
	for (int i = 1; i <= n; ++i)
		scanf("%d", &arr[i]);

	printf("%d\n", s());

	return 0;
}
