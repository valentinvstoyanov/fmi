#include <iostream>
#include <ctime>
#include "executor.h"
#include "wallet.h"
#include "transaction.h"
#include "order.h"

const unsigned FMICOIN_RATE = 375;

void transfer_fmi_coins(const unsigned sender_id, const unsigned receiver_id, const double fmi_coins) {
    Transaction transaction;
    create_transaction(transaction, sender_id, receiver_id, fmi_coins);
    add_transaction_to_cache(transaction);
}

void on_add_wallet() {
    Wallet wallet;
    read_wallet(wallet);
    add_wallet_to_cache(wallet);
    transfer_fmi_coins(SYSTEM_WALLET_ID, wallet.id, wallet.fiat_money / FMICOIN_RATE);
}

void on_make_order() {
    Order order;
    read_order(order);
    Wallet orderer;
    if(!find_wallet_by_id(order.wallet_id, orderer)) {
        std::cout << "Couldn't find wallet with id = " << order.wallet_id << std::endl;
        return;
    }
    double money;
    bool has_enough_money;
    bool selling = order.type == Order::SELL;
    money = (selling ? calculate_fmi_coins(orderer.id) : orderer.fiat_money);
    has_enough_money = (selling ? money >= order.fmi_coins : money >= order.fmi_coins * FMICOIN_RATE);

    int co_size;
    Order* completed_orders = complete_orders(order, co_size);
    if(completed_orders && co_size > 0) {
        if(order.fmi_coins > 0.001)
            add_order_to_cache(order);
            //TODO: update wallet coins
        if(selling) {
            for(size_t i = 0; i < co_size; ++i) {
                Order tmp = completed_orders[i];
                transfer_fmi_coins(orderer.id, tmp.wallet_id, tmp.fmi_coins);
            }
        } else {
            for(size_t i = 0; i < co_size; ++i) {
                Order tmp = completed_orders[i];
                transfer_fmi_coins(tmp.wallet_id, orderer.id, tmp.fmi_coins);

            }
        }
    } else {
        std::cout << "No completed orders" << std::endl;
        add_order_to_cache(order);
    }
    delete[] completed_orders;
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

}

void on_quit() {
    persist_wallets_cache();
    persist_transactions_cache();
    persist_orders_cache();
}
