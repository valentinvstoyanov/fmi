#include <iostream>
#include <queue>

using namespace std;

const int SIZE = 10;

void dfs(bool graph[SIZE][SIZE], int vertex_count, int start) {
	queue<int> q;
	bool visited[SIZE] = { false };
	q.push(start);
	visited[start] = true;

	while (!q.empty()) {
		int v = q.front();
		q.pop();

		cout << v << ' ';

		for (int i = 0; i < vertex_count; ++i) {
			if (graph[v][i] && !visited[i]) {
				visited[i] = true;
				q.push(i);
			}
		}
	}
}

int main() {
	bool graph[SIZE][SIZE] = { false };
	int vertex_count, edge_count;
	cin >> vertex_count >> edge_count;

	for (int i = 0; i < edge_count; ++i) {
		int v1, v2;
		cin >> v1 >> v2;
		graph[v1][v2] = true;
	}

	dfs(graph, vertex_count, 0);

	return 0;
}
