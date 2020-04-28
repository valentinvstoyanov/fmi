#include <iostream>
#include "random_coder.h"

int main() {
  RandomCoder coder;

  const auto t1 = "CRYPTOGRAPHY";
  const auto ct1 = coder.encode(t1);
  const auto dct1 = coder.decode(ct1);
  std::cout << "E(" << t1 << ") = " << ct1 << " | D(" << ct1 << ") = " << dct1 << std::endl;

  return 0;
}
