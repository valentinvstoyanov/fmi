//
// Created by valio_stoyanov on 12/12/18.
//

#include <fstream>
#include <iostream>
#include "parser.h"

bool ReadDance(const char* filename, Dance& dance) {
  std::ifstream file(filename);
  if (file.is_open()) {
    file >> dance;
    return true;
  }
  return false;
}

int main(int argc, char** argv) {
  const char* filename = argv[1];

  Dance dance;
  if (ReadDance(filename, dance)) {
    Handler handler(dance);
    Parser parser(handler);
    parser.Start();
  } else {
    std::cout << "Failed to open file: " << filename << std::endl;
  }

  return 0;
}