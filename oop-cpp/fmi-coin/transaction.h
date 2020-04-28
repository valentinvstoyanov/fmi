#ifndef __TRANSACTION_HEADER_INCLUDED__
#define __TRANSACTION_HEADER_INCLUDED__

struct Transaction {
    long long time;
    unsigned sender_id;
    unsigned receiver_id;
    double fmi_coins;
};

void print_transaction(const Transaction&);
void create_transaction(Transaction&, const unsigned, const unsigned, const double);
void add_transaction_to_cache(const Transaction&);
void persist_transactions_cache();
double calculate_fmi_coins(const unsigned);

#endif
