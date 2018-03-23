#ifndef __COMMAND_HEADER_INCLUDED__
#define __COMMAND_HEADER_INCLUDED__

const unsigned LONGEST_COMMAND = 17;

const char ADD_WALLET[] = "add-wallet";
const char MAKE_ORDER[] = "make-order";
const char WALLET_INFO[] = "wallet-info";
const char ATTRACT_INV[] = "attract-investors";
const char QUIT[] = "quit";

void read_command(char command[LONGEST_COMMAND]);
bool process_command(const char*);

#endif
