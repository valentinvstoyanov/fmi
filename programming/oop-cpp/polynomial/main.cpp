#include <iostream>
#include "ds/array.h"
#include "polynomial.h"

int main() {

  Array<int> p_coefficients(6);
  p_coefficients.PushBack(1);
  p_coefficients.PushBack(1);
  p_coefficients.PushBack(1);
  p_coefficients.PushBack(1);
  p_coefficients.PushBack(1);
  p_coefficients.PushBack(1);

  Polynomial<int> p(p_coefficients);

  Array<int> q_coefficients(2);
  q_coefficients.PushBack(1);
  q_coefficients.PushBack(1);

  Polynomial<int> q(q_coefficients);

  p%=q;
  std::cout << p << std::endl;

  return 0;
}