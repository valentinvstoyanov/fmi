#include<iostream>
#include<cmath>
#include<stack>
using namespace std;

int main() {
  stack<int> st;
  int n;
  while (cin >> n) {
    st.push(n);
  }

  while (!st.empty()) {
    printf("%.6f\n", sqrt(st.top()));
    st.pop();
  }

  return 0;
}