#include <iostream>
#include <cstring>
#include <ctime>
#include "command.h"
#include "performer.h"

void read_command(char command[LONGEST_COMMAND]) {
    std::cout << "Enter command: ";
    std::cin >> command;
}

bool process_command(const char* command) {
    if(!strcmp(command, ADD_WALLET)) {
        on_add_wallet();
    } else if(!strcmp(command, MAKE_ORDER)) {
        on_make_order();
    } else if(!strcmp(command, WALLET_INFO)) {
        on_wallet_info();
    } else if(!strcmp(command, ATTRACT_INV)) {
        on_attract_inv();
    } else if(!strcmp(command, QUIT)) {
        on_quit();
        return false;
    } else {
        std::cout << "No such command..." << std::endl;
    }

    return true;
}
