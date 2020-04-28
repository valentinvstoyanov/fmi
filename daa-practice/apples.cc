#include <iostream>
#include <queue>
#include <vector>
#include <unordered_set>

using namespace std;

const int SIZE = 100;
bool graph[SIZE][SIZE] = { false };
int N, M, T; 
vector<pair<int, int>> infected;

namespace std
{
	template<> struct hash<pair<int, int>>
	{
		size_t operator()(const pair<int, int>& p) const noexcept
		{
			size_t const h1 ( std::hash<int>{}(p.first) );
			size_t const h2 ( std::hash<int>{}(p.second) );
			return h1 ^ (h2 << 1);
		}
	};
}

void bfs() {
	queue<pair<int, int>> q;
	unordered_set<pair<int, int>> visited;

	for (auto& p: infected) {
		visited.insert(p);
		q.push(p);
	}

	int counter = 0;
	while (counter < T) {
		++counter;

		int current_visited = visited.size();
		for (int i = 0; i < current_visited; ++i) {
			pair<int, int> coord = q.front();
			q.pop();

			pair<int, int> right = make_pair(coord.first + 1, coord.second);	
			pair<int, int> left = make_pair(coord.first - 1, coord.second);	
			pair<int, int> top = make_pair(coord.first, coord.second + 1);   	
			pair<int, int> bottom = make_pair(coord.first, coord.second - 1);

			if (right.first <= M && visited.find(right) == visited.end()) {
				visited.insert(right);
				graph[right.first][right.second] = true;
				q.push(right);
			}
			if (left.first >= 0 && visited.find(left) == visited.end()) {
				visited.insert(left);
				graph[left.first][left.second] = true;
				q.push(left);
			}		
			if (top.second <= N && visited.find(top) == visited.end()) {
				visited.insert(top);
				graph[top.first][top.second] = true;
				q.push(top);
			}		
			if (bottom.second >= 0 && visited.find(bottom) == visited.end()) {
				visited.insert(bottom);
				graph[bottom.first][bottom.second] = true;
				q.push(bottom);
			}
		}
	}

	cout << (N * M) - visited.size() << endl;
}

int main() {
	cin >> N >> M >> T;

	int v1, v2;
	while (cin >> v1 >> v2) {
		graph[v1][v2] = true;
		infected.push_back(make_pair(v1, v2));
	}

	bfs();

	return 0;
}
