#include <iostream>
#include "ds/array.h"
#include "polynomial.h"

int main() {

  Array<int> p_coefficients(3);
  p_coefficients.PushBack(1);
  p_coefficients.PushBack(2);
  p_coefficients.PushBack(3);

  Polynomial<int> p(p_coefficients);

  Array<int> q_coefficients(3);
  q_coefficients.PushBack(1);
  q_coefficients.PushBack(1);
  q_coefficients.PushBack(1);

  Polynomial<int> q(q_coefficients);

  std::cout << p << std::endl;
  std::cout << q << std::endl;

  std::cout << !p << std::endl;

  return 0;
}