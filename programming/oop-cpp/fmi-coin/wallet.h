#ifndef __WALLET_HEADER_INCLUDED__
#define __WALLET_HEADER_INCLUDED__

const unsigned SYSTEM_WALLET_ID = 4294967295;
const unsigned short OWNER_SIZE = 256;

struct Wallet {
    unsigned id;
    char owner[OWNER_SIZE];
    double fiat_money;
};

void read_wallet(Wallet&);
void print_wallet(const Wallet&);
void add_wallet_to_cache(Wallet&);
bool find_wallet_by_id(const unsigned, Wallet&);
void persist_wallets_cache();

#endif
