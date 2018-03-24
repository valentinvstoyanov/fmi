#include <iostream>
#include <ctime>
#include <fstream>
#include "transaction.h"
#include "util.h"

static const char TRANSACTION_FILENAME[] = "Transactions.dat";
static const unsigned short CACHE_CAPACITY = 64;
static Transaction cache[CACHE_CAPACITY];
static unsigned short cache_size = 0;
static bool is_persisted = true;

void save_transaction(std::ofstream& file, const Transaction& transaction) {
    file.write(reinterpret_cast<const char*>(&transaction.time), sizeof(long long));
    file.write(reinterpret_cast<const char*>(&transaction.sender_id), sizeof(unsigned));
    file.write(reinterpret_cast<const char*>(&transaction.receiver_id), sizeof(unsigned));
    file.write(reinterpret_cast<const char*>(&transaction.fmi_coins), sizeof(double));
}

void load_transaction(std::ifstream& file, Transaction& transaction) {
    file.read(reinterpret_cast<char*>(&transaction.time), sizeof(long long));
    file.read(reinterpret_cast<char*>(&transaction.sender_id), sizeof(unsigned));
    file.read(reinterpret_cast<char*>(&transaction.receiver_id), sizeof(unsigned));
    file.read(reinterpret_cast<char*>(&transaction.fmi_coins), sizeof(double));
}

void create_transaction(Transaction& t, const unsigned sender_id, const unsigned receiver_id, const double fmi_coins) {
    t.time = time(NULL);
    t.sender_id = sender_id;
    t.receiver_id = receiver_id;
    t.fmi_coins = fmi_coins;
}

void add_transaction_to_cache(const Transaction& transaction) {
    cache[cache_size++] = transaction;
    is_persisted = false;

    if(cache_size == CACHE_CAPACITY)
        persist_transactions_cache();
}

void persist_transactions_cache() {
    if(cache_size == 0 || is_persisted)
        return;
    std::ofstream file;
    file.open(TRANSACTION_FILENAME, std::ios::binary | std::ios::app | std::ios::out);
        if(!file) {
            std::cerr << "Failed to open " << TRANSACTION_FILENAME << " file." << std::endl;
        } else {
            unsigned short i = 0;
            for(i; file && i < cache_size; ++i)
                save_transaction(file, cache[i]);
            file.close();
            is_persisted = file;
            if(file) {
                cache_size = 0;
            } else {
                cache_size -= i;
                for(unsigned short j = i; j < cache_size; ++j)
                    cache[j - i] = cache[j];
            }
        }
    }

double calculate_fmi_coins(const unsigned wallet_id) {
    double total_coins = 0.0;

    for(unsigned short i = 0; i < cache_size; ++i) {
        Transaction t = cache[i];
        if(t.sender_id == wallet_id || t.receiver_id == wallet_id)
            total_coins += (t.sender_id == wallet_id ? -t.fmi_coins : t.fmi_coins);
    }

    std::ifstream file;
    file.open(TRANSACTION_FILENAME, std::ios::binary | std::ios::in);
    if(file) {
        const size_t file_size = get_file_size(file);
        const size_t transaction_size = sizeof(long long) + 2*sizeof(unsigned) + sizeof(double);
        for (size_t i = 0; file && i < file_size; i += transaction_size) {
            Transaction transaction;
            load_transaction(file, transaction);
            if(wallet_id == transaction.sender_id)
                total_coins -= transaction.fmi_coins;
            else if(wallet_id == transaction.receiver_id)
                total_coins += transaction.fmi_coins;
        }

        file.close();
    }

    return total_coins;
}
