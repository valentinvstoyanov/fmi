#include <iostream>
using namespace std;

const int MAX_SIZE = 100;

int main() {
	int n;
	cin >> n;	

	int arr[MAX_SIZE][MAX_SIZE];
	int counter = 0;

	int top = 0;
	int bottom = n - 1;
	int left = 0;
	int right = n - 1;
	
	int direction = 0;

	while (top <= bottom && left <= right) {
		if (direction == 0) {
			for (int i = left; i <= right; ++i) {
				arr[top][i] = ++counter;
			}
			++top;
		} else if (direction == 1) {
			for (int i = top; i <= bottom; ++i) {
				arr[i][right] = ++counter;
			}
			--right;
		} else if (direction == 2) {
			for (int i = right; i >= left; --i) {
				arr[bottom][i] = ++counter;
			}
			--bottom;
		} else if (direction == 3) {
			for (int i = bottom; i >= top; --i) {
				arr[i][left] = ++counter;
			}
			++left;
		}

		++direction %= 4;
	}

	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < n; ++j) {
			cout << arr[i][j] << (j == n - 1 ? "" : " ");
		}
		cout << endl;
	}

	return 0;
}
