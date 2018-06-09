#include <iostream>
#include "ds/array.h"
#include "polynomial.h"


void Example1() {
  std::cout << "EXAMPLE 1\n\n" << std::endl;

  Array<double> p_coefficients(0);
  Polynomial<double> p(p_coefficients);

  Array<double> q_coefficients(5);
  q_coefficients.PushBack(0.5);
  q_coefficients.PushBack(2);
  q_coefficients.PushBack(4);
  q_coefficients.PushBack(6);
  q_coefficients.PushBack(8);
  Polynomial<double> q(q_coefficients);

  std::cout << "p = " << p << std::endl;
  std::cout << "q = " << q << std::endl;

  std::cout << "p + q = " << p + q << std::endl;
  std::cout << "p - q = " << p - q << std::endl;
  std::cout << "p * q = " << p * q << std::endl;

  try {
    std::cout << p / q << std::endl;
  } catch (const std::invalid_argument& ex) {
    std::cout << ex.what() << std::endl;
  }

  try {
    std::cout << p % q << std::endl;
  } catch (const std::invalid_argument& ex) {
    std::cout << ex.what() << std::endl;
  }

  std::cout << "p[0] = " << p[0] << std::endl;
  std::cout << "q[3] = " << q[3] << std::endl;
  std::cout << "p(10) = " << p(10) << std::endl;
  std::cout << "q(10) = " << q(10) << std::endl;
  std::cout << "p(1, 10) = " << p(1, 10) << std::endl;
  std::cout << "q(1, 10) = " << q(1, 10) << std::endl;
  std::cout << "++p = " << ++p << std::endl;
  std::cout << "--q = " << --q << std::endl;
  std::cout << "p zero ? = " << (bool)p << std::endl;
  std::cout << "q zero ? = " << (bool)q << std::endl;
  std::cout << "p non-zero ? = " << !p << std::endl;
  std::cout << "q non-zero ? = " << !q << std::endl;
}

void Example2() {
  std::cout << "EXAMPLE 2\n\n" << std::endl;

  Array<double> p_coefficients(3);
  p_coefficients.PushBack(1);
  p_coefficients.PushBack(1);
  p_coefficients.PushBack(1);
  Polynomial<double> p(p_coefficients);

  Array<double> q_coefficients;
  q_coefficients.PushBack(1);
  q_coefficients.PushBack(1);
  Polynomial<double> q(q_coefficients);

  std::cout << "p = " << p << std::endl;
  std::cout << "q = " << q << std::endl;

  p += q;
  std::cout << "p += q = " << p << std::endl;

  p -= q;
  std::cout << "p -= q = " << p << std::endl;

  p *= q;
  std::cout << "p *= q = " << p << std::endl;

  std::cout << "deg(p) = " << (unsigned) p << std::endl;
  std::cout << "deg(q) = " << (unsigned) q << std::endl;
}

void Example3() {

}

int main() {
  Example1();

  std::cout << "----------------------------------------------------------\n\n";

  Example2();

  std::cout << "----------------------------------------------------------\n\n";

  Example3();

  return 0;
}