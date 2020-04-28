#include<iostream>
using namespace std;

int gcd(int a, int b) {
	return b == 0 ? a : gcd(b, a % b);
}

struct Rational {
  public:
	Rational(int p1 = 1, int q1 = 1) { p = p1; q = q1; }
	int p, q;

	bool operator>(const Rational& rhs) {
		return p * rhs.q > q * rhs.p;
	}
};

Rational arr[5000];
int counter = 0;

void insertSorted(const Rational& r) {
	int i;
	for (i = counter - 1; i >= 0 && arr[i] > r; --i) arr[i + 1] = arr[i];
	arr[i + 1] = r;	
	++counter;
}

int main() {
	int n;
	scanf("%d", &n);
	printf("0");

	for (int p = 1; p <= n; ++p) {
		for (int q = 2; q <= n; ++q) {
			if (p >= q || gcd(q, p) != 1) continue;
			Rational r = {p, q};
			insertSorted(r);
		}
	}

	for (int i = 0; i < counter; ++i)
		printf(" %d/%d", arr[i].p, arr[i].q);

	printf("\n");
	
	return 0;
}
