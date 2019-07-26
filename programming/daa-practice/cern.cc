#include<iostream>
using namespace std;

const int MAX_SIZE = 10000;
const int MAX_NUMBER_LEN = 8;

class Number {
  public:
	char arr[MAX_NUMBER_LEN];
	
	Number& operator=(const Number& other) {
		if (this != &other) {
			for (int i = 0; i < MAX_NUMBER_LEN; ++i)
				arr[i] = other.arr[i];
		}

		return *this;
	}

	bool operator<(const Number& other) const {
		const char* this_ptr = arr;
		const char* other_ptr = other.arr;
		while (*this_ptr && *this_ptr == *other_ptr) {
			++this_ptr;
			++other_ptr;
		}
		return *this_ptr - *other_ptr < 0;	
	}
};

void insertSorted(Number* arr, int size, Number num) {
	arr[size] = num;
	for (int i = size - 1; i >= 0; --i) {
		if (arr[i] < num) {
			break;
		} else {
			swap(arr[i], arr[i + 1]);
		}	
	}
}

int main() {
	int n;
	cin >> n;

	Number arr[MAX_SIZE];
	Number num;
	for (int i = 0; i < n; ++i) {
		cin >> num.arr;
		insertSorted(arr, i, num);
	}

	for (int i = 0; i < n; ++i) {
		cout << arr[i].arr << '\n';
	}

	return 0;
}
