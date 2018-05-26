#include <iostream>
#include "fmibook.h"
#include "client.h"
#include "data/model/text_post.h"

int main() {
  String admin_nickname("valio");

  User admin(admin_nickname, 20, User::Role::kAdmin);
  Fmibook fmibook(admin);
  Client client(fmibook);

  const unsigned kMaxInputLen = 2048;
  char input[kMaxInputLen];
  bool running = true;

  do {
    std::cin.getline(input, kMaxInputLen);
    String in(input);
    running = client.ProcessInput(in);
  } while(running);

  return 0;
}