#include<iostream>
using namespace std;

class Date {
 public:
  int day, month, year;
  int idx;
  Date() {}
  Date(int idx, int day, int month, int year) : idx(idx), day(day), month(month), year(year) {}

  bool operator<(const Date& other) {
    if (year != other.year) {
      return year < other.year;
    }

    if (month != other.month) {
      return month < other.month;
    }

    if (day != other.day) {
      return day < other.day;
    }

    return idx < other.idx;
  }
};

void selection_sort(Date* arr, int size) {
  for (int i = 0; i < size - 1; ++i) {
    int min_idx = i;
    for (int j = i + 1; j < size; ++j) {
      if (arr[j] < arr[min_idx]) {
        min_idx = j;
      }
    }
    if (min_idx != i) {
      swap(arr[i], arr[min_idx]);
    }
  }
}

int main() {
  int n;
  cin >> n;
  Date dates[100];

  int d, m, y;
  for (int i = 0; i < n; ++i) {
    cin >> d;
    cin.ignore();
    cin >> m;
    cin.ignore();
    cin >> y;
    dates[i] = Date(i, d, m, y);
  }

  selection_sort(dates, n);

  for (int i = 0; i < n; ++i) {
    Date& date = dates[i];
    cout << date.idx + 1 << endl;
  }
  
  return 0;
}
