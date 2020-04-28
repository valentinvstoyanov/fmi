#include "command.h"

int main() {
    bool running = true;
    do {
        char command[LONGEST_COMMAND];
        read_command(command);
        running = process_command(command);
    } while(running);

    return 0;
}


























/*
const unsigned short LONGEST_COMMAND = 17;

void parse_input_command(char* command) {

}

void transfer_coins(const Wallet& sender, const Wallet& receiver, const double
fmi_coins) {
    const Transaction transaction = {time(NULL), sender.id, receiver.id, fmi_coins};
    add_transaction_to_cache(transaction);
}

const unsigned fmi_coin_rate = 375;
const char cmd_add_wallet[] = "add-wallet";
const char cmd_make_order[] = "make-order";
const char cmd_wallet_info[] = "wallet-info";
const char cmd_attract_investors[] = "attract-investors";
const char cmd_quit[] = "quit";


char input_cmd[LONGEST_COMMAND];
bool is_cmd_valid;
bool running = true;

do {
std::cin >> input_cmd;
is_cmd_valid = true;

if(strcmp(input_cmd, cmd_add_wallet) == 0) {
  Wallet wallet;
  read_wallet(wallet);
  add_wallet_to_cache(wallet);
  Wallet system_wallet = {
SYSTEM_WALLET_ID,
"FMICOIN SYSTEM WALLET",
wallet.fiat_money
  };
  transfer_coins(
system_wallet,
wallet,
system_wallet.fiat_money / fmi_coin_rate
  );
} else if(strcmp(input_cmd, cmd_make_order) == 0) {
  Order order;
  read_order(order);
  Wallet orderer;
  if(find_wallet_by_id(order.wallet_id, orderer)) {
if(order.type == Order::SELL) {
    double wallet_fmi_coins =
    calculate_wallet_fmi_coins(orderer.id);
    if(wallet_fmi_coins >= order.fmi_coins) {

    } else {
  std::cout << "You have not enough FMICoins" <<
  std::endl;
    }
}
  } else {
std::cout << "Couldn't find wallet with such id, please \
check the id and try again." << std::endl;
  }
} else if(strcmp(input_cmd, cmd_wallet_info) == 0) {
} else if(strcmp(input_cmd, cmd_attract_investors) == 0) {
} else if(strcmp(input_cmd, cmd_quit) == 0) {
  persist_wallets_cache();
  persist_transactions_cache();
  running = false;
} else {
  is_cmd_valid = false;
  std::cout << "Invalid command" << std::endl;
}
} while(running || !is_cmd_valid);

*/
