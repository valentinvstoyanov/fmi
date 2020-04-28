#include <iostream>

using namespace std;

const int MAX_SIZE = 10000000;
int n;
char arr[MAX_SIZE] = {0};
char res[MAX_SIZE] = {0};
unsigned count[63] = {0};

inline unsigned idxOf(char c) {
	if (c >= '1' && c <= '9') return (c - '0') + 1;
	if (c >= 'a' && c <= 'z') return c - 86;
	if (c >= 'A' && c <= 'Z') return c - 28;
	printf("Wtf: %c ?", c);
	throw "Impossible";
}

int main() {
	scanf("%d", &n);
	scanf("%s", arr);

	for (int i = 0; i < n; ++i) ++count[idxOf(arr[i])];
	for (int i = 1; i < 63; ++i) count[i] += count[i - 1];
	for (int i = n - 1; i >= 0; --i) {
		auto& cnt = count[idxOf(arr[i])];
		res[cnt - 1] = arr[i];
		--cnt;
	}

	printf("%s\n", res);
	
	return 0;
}
