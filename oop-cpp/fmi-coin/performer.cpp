#include <iostream>
#include <ctime>
#include "performer.h"
#include "wallet.h"
#include "transaction.h"
#include "order.h"
#include "util.h"

const unsigned FMICOIN_RATE = 375;

void transfer_fmi_coins(const unsigned sender_id, const unsigned receiver_id, const double fmi_coins) {
    Transaction transaction;
    create_transaction(transaction, sender_id, receiver_id, fmi_coins);
    add_transaction_to_cache(transaction);
    if(sender_id == SYSTEM_WALLET_ID)
        std::cout << "BONUS ";
    print_transaction(transaction);
}

void on_add_wallet() {
    Wallet wallet;
    read_wallet(wallet);
    add_wallet_to_cache(wallet);
    std::cout << "CREATED ";
    print_wallet(wallet);
    const double bonus_fmi_coins = wallet.fiat_money / FMICOIN_RATE;
    std::cout << "\tFMICOINS: " << bonus_fmi_coins << std::endl;
    transfer_fmi_coins(SYSTEM_WALLET_ID, wallet.id, bonus_fmi_coins);
}

void on_make_order() {
    Order order;
    read_order(order);
    Wallet orderer;
    if(!find_wallet_by_id(order.wallet_id, orderer)) {
        std::cout << "Couldn't find wallet with id = " << order.wallet_id << std::endl;
        return;
    }

    bool has_enough_money;
    if(order.type == Order::SELL)
        has_enough_money = double_cmp(calculate_fmi_coins(order.wallet_id), order.fmi_coins) >= 0;
    else
        has_enough_money = double_cmp(orderer.fiat_money, (order.fmi_coins * FMICOIN_RATE)) >= 0;

    if(has_enough_money) {
        size_t completed_orders_size = 4;
        const double order_fmi_coins = order.fmi_coins;
        Order* completed_orders = complete_orders(order, completed_orders_size);
        if(completed_orders) {
            if(completed_orders_size > 0) {
                const size_t update_wallets_arr_size = (order.fmi_coins < EPSILON ? completed_orders_size + 1 : completed_orders_size);
                size_t update_wallets_arr_counter = 0;

                unsigned* wallet_ids = new(std::nothrow) unsigned[update_wallets_arr_size];
                if(!wallet_ids) {
                    std::cerr << "Failed to allocate dynamic memory for wallet ids." << std::endl;
                    delete[] completed_orders;
                    return;
                }

                double* wallet_fiat_money = new(std::nothrow) double[update_wallets_arr_size];
                if(!wallet_fiat_money) {
                    std::cerr << "Failed to allocate dynamic memory for wallet fiat money." << std::endl;
                    delete[] completed_orders;
                    return;
                }

                if(order.fmi_coins < EPSILON) {
                    wallet_ids[0] = order.wallet_id;
                    const double fiat_money = order_fmi_coins * FMICOIN_RATE;
                    wallet_fiat_money[0] = (order.type == Order::SELL ? fiat_money : -fiat_money);
                    update_wallets_arr_counter++;
                } else {
                    add_order_to_cache(order);
                }

                for(size_t i = 0; i < completed_orders_size; ++i) {
                    Order cur_order = completed_orders[i];
                    unsigned sender_id, receiver_id;
                    if(cur_order.type == Order::SELL) {
                        sender_id = cur_order.wallet_id;
                        receiver_id = order.wallet_id;
                    } else {
                        sender_id = order.wallet_id;
                        receiver_id = cur_order.wallet_id;
                    }

                    //TODO: create file for each transaction
                
                    transfer_fmi_coins(sender_id, receiver_id, cur_order.fmi_coins);
                    const double fiat_money = (cur_order.type == Order::SELL ? cur_order.fmi_coins * FMICOIN_RATE : -cur_order.fmi_coins * FMICOIN_RATE);
                    insert_sorted(wallet_ids, wallet_fiat_money, update_wallets_arr_counter, update_wallets_arr_size, cur_order.wallet_id, fiat_money);
                    update_wallets_arr_counter++;
                }

                update_fiat_money(wallet_ids, wallet_fiat_money, update_wallets_arr_size);

                delete[] wallet_ids;
                delete[] wallet_fiat_money;
            } else {
                add_order_to_cache(order);
                std::cout << "Order added to the waiting list." << std::endl;
            }
            delete[] completed_orders;
            completed_orders = NULL;
        }
    }
}

void on_wallet_info() {
    unsigned wallet_id;
    std::cin >> wallet_id;
    Wallet wallet;
    if(!find_wallet_by_id(wallet_id, wallet)) {
        std::cout << "Couldn't find wallet with id = " << wallet_id << std::endl;
        return;
    }
    print_wallet(wallet);
    std::cout << "\tFMICOINS: " << calculate_fmi_coins(wallet_id) << std::endl;
}

void on_attract_inv() {
    //TODO: iterate through all the wallets and find top 10
}

void on_quit() {
    persist_wallets_cache();
    persist_transactions_cache();
    persist_orders_cache();
}
