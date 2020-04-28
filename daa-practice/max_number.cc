#include <iostream>
#include <cstring>
#include <algorithm>
using namespace std;

const int MAX_SIZE = 100;
int n;

struct Num {
  char digits[7] ={0};

  Num& operator=(const Num& other) {
    if (this != &other) {
      strcpy(digits, other.digits);
    }

    return *this;
  }
  
  bool gr(const char* p1, const char* p2) const {
	char c1[14];
	char c2[14];
	strcat(strcpy(c1, p1), p2);
	strcat(strcpy(c2, p2), p1);
	if(lexicographical_compare(c1, c1+14, c2, c2+14)) return false;
	else return true;
  }

  bool operator>(const Num& other) const {
    return gr(digits, other.digits);
  }
};

Num arr[MAX_SIZE];

void insertSorted(const Num& num, int size) {
  int j = size - 1;
  while (j >= 0 && num > arr[j]) {
    arr[j + 1] = arr[j];
    --j;
  }
  arr[j + 1] = num;
}

int main() {
  scanf("%d", &n);
  Num num;
  for (int i = 0; i < n; ++i) {
    scanf("%7s", num.digits);
    insertSorted(num, i);
  }

  for (int i = 0; i < n; ++i) {
    printf("%s", arr[i].digits);
  }
  printf("\n");

  return 0;
}
