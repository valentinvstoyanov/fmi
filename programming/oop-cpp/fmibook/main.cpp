#include <iostream>
#include "fmibook.h"
#include "client.h"
#include "data/model/text_post.h"
#include "data/model/link_post.h"

int main() {
  String admin_nickname("valio");

  User admin(admin_nickname, 20, User::Role::kAdmin);
  Fmibook fmibook(admin);
  Client client(fmibook);

  const unsigned kMaxInputLen = 2048;
  char input[kMaxInputLen];
  bool running = true;


 /* PostArray a;
  TextPost tp(admin_nickname);
  a.PushBack(tp);
  LinkPost lp(admin_nickname, admin_nickname);
  a.PushBack(lp);
  for (size_t i = 0; i < a.Size(); ++i) {
    std::cout << a.At(i).toHtml() << std::endl;
  }
*/
  do {
    std::cin.getline(input, kMaxInputLen);
    String in(input);
    running = client.ProcessInput(in);
  } while(running);

  return 0;
}