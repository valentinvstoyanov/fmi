#include <iostream>
#include <fstream>
#include "wallet.h"
#include "util.h"

void read_wallet(Wallet& wallet) {
    std::cin >> wallet.fiat_money;
    std::cin >> wallet.owner;
}

void print_wallet(const Wallet& wallet) {
    std::cout << "WALLET: " << std::endl;
    std::cout << "\tID: " << wallet.id << std::endl;
    std::cout << "\tOWNER: " << wallet.owner << std::endl;
    std::cout << "\tFIAT MONEY:  " << wallet.fiat_money << std::endl;
}

void load_wallet(std::istream& file, Wallet& wallet) {
    file.read(reinterpret_cast<char*>(&wallet.id), sizeof(unsigned));
    file.read(reinterpret_cast<char*>(&wallet.owner), OWNER_SIZE);
    file.read(reinterpret_cast<char*>(&wallet.fiat_money), sizeof(double));
}

void save_wallet(std::ofstream& file, const Wallet& wallet) {
    file.write(reinterpret_cast<const char*>(&wallet.id), sizeof(unsigned));
    file.write(reinterpret_cast<const char*>(&wallet.owner), OWNER_SIZE);
    file.write(reinterpret_cast<const char*>(&wallet.fiat_money), sizeof(double));
}

static const unsigned WALLET_SIZE = sizeof(unsigned) + sizeof(double) + OWNER_SIZE;
static const char WALLET_FILENAME[] = "Wallets.dat";
static const unsigned short CACHE_CAPACITY = 10;
static Wallet cache[CACHE_CAPACITY];
static unsigned short cache_size = 0;
static bool is_persisted = true;

unsigned next_wallet_id() {
    static unsigned wallet_id = 0;
    static bool is_file_checked = false;
    std::ifstream file;
    file.open(WALLET_FILENAME, std::ios::binary | std::ios::in);

    if(!is_file_checked && file) {
        file.seekg(get_file_size(file) - WALLET_SIZE, std::ios::beg);
        Wallet wallet;
        load_wallet(file, wallet);
        file.close();
        wallet_id = wallet.id + 1;
        is_file_checked = true;
    }

    if(wallet_id == SYSTEM_WALLET_ID)
        wallet_id++;

    return wallet_id++;
}

void add_wallet_to_cache(Wallet& wallet) {
    wallet.id = next_wallet_id();
    cache[cache_size++] = wallet;
    is_persisted = false;

    if(cache_size == CACHE_CAPACITY)
        persist_wallets_cache();
}

bool search_wallet_cache(const unsigned id, Wallet& wallet) {
    unsigned left = 0;
    unsigned right = cache_size - 1;

    while(left <= right) {
        unsigned middle = left + (right - left) / 2;

        if(cache[middle].id == id) {
            wallet = cache[middle];
            return true;
        }

        if(id < cache[middle].id)
            right = middle - 1;
        else
            left = middle + 1;
    }

    return false;
}

int search_wallet_pos_at_file(std::ifstream& file, const unsigned id) {
    if(!file) {
        std::cerr << "Failed to open file " << WALLET_FILENAME << std::endl;
        return -1;
    } else {
        const size_t u_size = sizeof(unsigned);
        const size_t d_size = sizeof(double);
        const size_t file_size = get_file_size(file);

        long long left = 0;
        long long right = file_size - WALLET_SIZE;

        while(left <= right && file) {
            long long middle = left + (right - left) / 2;
            middle -= middle % WALLET_SIZE;
            file.seekg(middle);

            unsigned read_id;
            file.read(reinterpret_cast<char*>(&read_id), u_size);
            if(read_id == id)
                return (size_t)file.tellg() - u_size;

            if(id < read_id)
                right = middle - WALLET_SIZE;
            else
                left = middle + WALLET_SIZE ;
        }

        return -1;
    }
}

bool search_wallet_file(const unsigned id, Wallet& wallet) {
    std::ifstream file;
    file.open(WALLET_FILENAME, std::ios::binary | std::ios::in);
    int pos = search_wallet_pos_at_file(file, id);
    if(pos >= 0) {
        file.seekg(pos + sizeof(unsigned), std::ios::beg);
        wallet.id = id;
        file.read(reinterpret_cast<char*>(&wallet.owner), OWNER_SIZE);
        file.read(reinterpret_cast<char*>(&wallet.fiat_money), sizeof(double));
        file.close();
        return file;
    } else {
        file.close();
        return false;
    }
}

bool find_wallet_by_id(const unsigned id, Wallet& wallet) {
    if(cache_size > 0) {
        Wallet first_cache = cache[0];
        if(first_cache.id < id) {
            if(search_wallet_cache(id, wallet))
                return true;
            else
                return search_wallet_file(id, wallet);
        } else if(first_cache.id == id) {
            wallet = first_cache;
            return true;
        } else {
            return search_wallet_file(id, wallet);
        }
    } else {
        return search_wallet_file(id, wallet);
    }
}


void update_fiat_money(const unsigned* const ids, const double* const fiat_money, const size_t size) {
    std::fstream file(WALLET_FILENAME, std::ios::binary | std::ios::in | std::ios::app | std::ios::out);
    if(!file) {
        std::cerr << "Failed to open " << WALLET_FILENAME << " file." << std::endl;
        const size_t file_size = get_file_size(file);
        const size_t fiat_money_seek = sizeof(unsigned) + OWNER_SIZE;
        for(size_t i = 0; file && i < file_size; i += WALLET_SIZE) {
            Wallet tmp;
            load_wallet(file, tmp);
            int pos = binary_search(ids, size, tmp.id);
            if(pos >= 0) {
                tmp.fiat_money += fiat_money[pos];
                file.seekp(i + fiat_money_seek, std::ios::beg);
                file.write(reinterpret_cast<const char*>(&tmp.fiat_money), sizeof(double));
            }
        }
        file.close();
    }
    for(unsigned short i = 0; i < cache_size; ++i) {
        Wallet tmp = cache[i];
        int pos = binary_search(ids, size, tmp.id);
        if(pos >= 0) {
            tmp.fiat_money += fiat_money[pos];
            cache[i] = tmp;
        }
    }
}

void persist_wallets_cache() {
    if(cache_size == 0 || is_persisted)
        return;
    std::ofstream file;
    file.open(WALLET_FILENAME, std::ios::binary | std::ios::app | std::ios::out);
    if(!file) {
        std::cerr << "Failed to open " << WALLET_FILENAME << " file." << std::endl;
        return;
    }
    unsigned short i = 0;
    for(i; file && i < cache_size; ++i)
        save_wallet(file, cache[i]);
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
