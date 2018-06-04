#include <iostream>
#include "ds/array.h"
#include "polynomial.h"

int main() {
  Array<int> coeffs(5);
  for (size_t i = 0; i < 5; ++i)
    coeffs.PushBack(static_cast<const int&>(i));

  Polynomial<int> p(coeffs);

  coeffs.PushBack(60);
  Polynomial<int> q(coeffs);

  p -= q;

  std::cout << (int) p << std::endl;


  return 0;
}