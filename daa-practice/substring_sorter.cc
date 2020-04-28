#include <iostream>
#include <cstring>
#include <algorithm> 
using namespace std;

const int MAX_SIZE = 50;

int n, l;
char str[MAX_SIZE] = {0};

int main() {
	scanf("%d %d", &n, &l);	
	scanf("%50s", str);

	char min[MAX_SIZE];
	strcpy(min, str);
	sort(min, min + l);

	for (int i = 1; i < n - l; ++i) {
		char tmp[MAX_SIZE];
		strcpy(tmp, str); 
		sort(tmp + i, tmp + i + l);
		if (lexicographical_compare(tmp, tmp + n, min, min + n)) strcpy(min, tmp);	
	}

	printf("%s\n", min);

	return 0;
}
