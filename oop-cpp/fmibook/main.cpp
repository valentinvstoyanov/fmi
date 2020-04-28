#include <iostream>
#include "fmibook.h"
#include "client.h"
#include "data/repository/user_repository.h"

Fmibook BuildFmibook() {
  const UserRepository& kUserRepo = UserRepository::Instance();
  if (kUserRepo.HasRecords()) {
    try {
      return Fmibook::From(kUserRepo.LoadAdmin(),
                           kUserRepo.LoadModerators(),
                           kUserRepo.LoadUsers());
    } catch (const UserRepository::LoadException&) {
      return Fmibook::From(Client::ReadAdmin());
    }
  } else {
    return Fmibook::From(Client::ReadAdmin());
  }
}

int main() {
  Fmibook fmibook(BuildFmibook());
  Client client(fmibook);
  while(client.ProcessInput(Client::ReadInput()));

  return 0;
}