#include <iostream>
using namespace std;

const int MAX_SIZE = 501;
bool matrix[MAX_SIZE][MAX_SIZE] = {false};
bool visited[MAX_SIZE] = {false};
int n, m;

void dfs(int x) {
	visited[x] = true;
	for (int i = 1; i <= n; ++i) {
		if (matrix[x][i] && !visited[i]) {
			dfs(i);
		}
	}
}

int main() {
	scanf("%d %d", &n, &m);

	int a, b, res = 0;
	for (int i = 0; i < m; ++i) {
		scanf("%d %d", &a, &b);
		matrix[a][b] = matrix[b][a] = true;
	}
	
	for (int i = 1; i <= n; ++i) {
		if (!visited[i]) {
			dfs(i);
			++res;
		}
	}

	printf("%d\n", res);

	return 0;
}
