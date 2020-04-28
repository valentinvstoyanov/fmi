#include<iostream>
using namespace std;

const int MAX_SIZE = 20;

void read_matrix(int matrix[MAX_SIZE][MAX_SIZE], int rows, int cols) {
	for (int i = 0; i < rows; ++i) {
		for (int j = 0; j < cols; ++j) {
			cin >> matrix[i][j];
		}
	}
}

int main() {
	int m, n, k;
	cin >> m >> n >> k;

	int matrix1[MAX_SIZE][MAX_SIZE];
	int matrix2[MAX_SIZE][MAX_SIZE];
	
	read_matrix(matrix1, m, n);
	read_matrix(matrix2, n, k);

	for (int i = 0; i < m; ++i) {
		for (int j = 0; j < k; ++j) {
			int sum = 0;
			for (int c = 0; c < n; ++c) {
				sum += matrix1[i][c] * matrix2[c][j];	
			}
			cout << sum << (j == k - 1 ? "" : " ");		
		}
		cout << endl;
	}

	return 0;
}
