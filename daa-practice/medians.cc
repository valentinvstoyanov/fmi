#include<iostream>
using namespace std;

const int MAX_SIZE = 10000;
int n;
int arr[MAX_SIZE];

int counter = 0;
void insertSorted(int e) {
  int j = counter - 1;
  while (j >= 0 && arr[j] > e) {
    arr[j + 1] = arr[j];
    --j;
  }
  arr[j + 1] = e;
  ++counter;
}

int main() {
  scanf("%d", &n);
  int e;
  for (int i = 0; i < n; ++i) {
    scanf("%d", &e);
    insertSorted(e);
    if ((i + 1) % 2 == 0) {
      printf("%.1f ", ((double)arr[i/2] + arr[i/2 + 1])/2);
    } else {
      printf("%.1f ", (double)arr[i/2]);
    }
  }

  printf("\n");

  return 0;
}
