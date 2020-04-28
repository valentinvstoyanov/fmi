#include <iostream>
#include <fstream>
#include <cstring>
#include <unistd.h>
#include <cmath>
#include "order.h"
#include "util.h"

void read_order(Order& order) {
    char order_type[4];
    do {
        std::cin >> order_type;
        if(!strcmp("SELL", order_type))
            break;
        if(!strcmp("BUY", order_type))
            break;
    } while(true);
    if(order_type[0] == 'S')
        order.type = Order::SELL;
    else
        order.type = Order::BUY;

    std::cin >> order.fmi_coins;
    std::cin >> order.wallet_id;
}

void save_order(std::ofstream& file, const Order& o) {
    file.write(reinterpret_cast<const char*>(&o.type), sizeof(Order::Type));
    file.write(reinterpret_cast<const char*>(&o.wallet_id), sizeof(unsigned));
    file.write(reinterpret_cast<const char*>(&o.fmi_coins), sizeof(double));
}

void load_order(std::istream& file, Order& o) {
    file.read(reinterpret_cast<char*>(&o.type), sizeof(Order::Type));
    file.read(reinterpret_cast<char*>(&o.wallet_id), sizeof(unsigned));
    file.read(reinterpret_cast<char*>(&o.fmi_coins), sizeof(double));
}

static const char ORDER_FILENAME[] = "Orders.dat";
static const unsigned short CACHE_CAPACITY = 64;
static Order cache[CACHE_CAPACITY];
static unsigned short cache_size = 0;
static bool is_persisted = true;

void add_order_to_cache(const Order& order) {
    cache[cache_size++] = order;
    is_persisted = false;

    if(cache_size == CACHE_CAPACITY)
        persist_orders_cache();
}

void persist_orders_cache() {
    if(cache_size == 0 || is_persisted)
        return;
    std::ofstream file;
    file.open(ORDER_FILENAME, std::ios::binary | std::ios::app | std::ios::out);
    if(!file) {
        std::cerr << "Failed to open " << ORDER_FILENAME << " file." << std::endl;
    } else {
        unsigned short i = 0;
        for(i; i < cache_size; ++i)
            save_order(file, cache[i]);
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

bool delete_orders_at_positions(const size_t* positions, const size_t pos_size, std::fstream& file, const size_t file_size) {
    const char* tmp_filename = "ORDERS_TMP.dat";
    std::ofstream tmp_file(tmp_filename, std::ios::binary | std::ios::out | std::ios::trunc);
    file.seekg(0, std::ios::beg);
    for(size_t current_pos = 0; file && tmp_file && current_pos < file_size; current_pos += ORDER_SIZE) {
        if(binary_search(positions, pos_size, current_pos) < 0) {
            Order tmp;
            load_order(file, tmp);
            save_order(tmp_file, tmp);
        } else {
            file.seekg(ORDER_SIZE, std::ios::cur);
        }
    }
    file.close();
    tmp_file.close();
    return file && tmp_file && !unlink(ORDER_FILENAME) && !rename(tmp_filename, ORDER_FILENAME);
}

void delete_order_cache(const size_t index) {
    for (int i = index; i < cache_size; ++i)
        cache[i] = cache[i + 1];
    cache_size--;
}

Order* ensure_orders_size(Order* arr, const size_t size, size_t& capacity, size_t new_elements) {
    if(new_elements + size - 1 <  capacity)
        return arr;
    const size_t new_capacity = max(2 *  capacity, new_elements + size - 1);
    Order* old_arr = arr;
    arr = new(std::nothrow) Order[new_capacity];
    if(!arr) {
        delete[] old_arr;
        old_arr = NULL;
        return NULL;
    }
    for(size_t i = 0; i < size; ++i)
        arr[i] = old_arr[i];

    delete[] old_arr;
    capacity = new_capacity;

    return arr;
}

size_t* ensure_indexes_size(size_t* arr, const size_t size, size_t& capacity, size_t new_elements) {
    if(new_elements + size - 1 <  capacity)
        return arr;
    const size_t new_capacity = max(2 *  capacity, new_elements + size - 1);
    size_t* old_arr = arr;
    arr = new(std::nothrow) size_t[new_capacity];
    if(!arr) {
        delete[] old_arr;
        old_arr = NULL;
        return NULL;
    }
    for(size_t i = 0; i < size; ++i)
        arr[i] = old_arr[i];

    delete[] old_arr;
    capacity = new_capacity;

    return arr;
}

Order* add_order_to_completed(Order* completed_orders, size_t& size, size_t& capacity, const Order& order) {
    for (size_t i = 0; i < size; ++i) {;
        if (order.wallet_id == completed_orders[i].wallet_id) {
            completed_orders[i].fmi_coins += order.fmi_coins;
            return completed_orders;
        }
    }
    completed_orders = ensure_orders_size(completed_orders, size, capacity, 1);
    if(!completed_orders)
        return NULL;
    completed_orders[size++] = order;
    return completed_orders;
}

Order* complete_orders(Order& order, size_t& result_arr_size) {
    size_t co_capacity = 4;
    size_t co_size = 0;
    Order* completed_orders = new(std::nothrow) Order[co_capacity];
    if(!completed_orders) {
        std::cerr << "Failed to allocate dynamic memory for completed orders." << std::endl;
        return NULL;
    }

    std::fstream file(ORDER_FILENAME, std::ios::binary | std::ios::in | std::ios::app | std::ios::out);
    if(!file) {
        std::cerr << "Failed to open " << ORDER_FILENAME << " file." << std::endl;
        delete[] completed_orders;
        completed_orders = NULL;
        return NULL;
    }

    size_t di_capacity = 2;
    size_t di_size = 0;
    size_t* delete_indexes = new(std::nothrow) size_t[di_capacity];
    if(!delete_indexes) {
        std::cerr << "Failed to allocate dynamic memory for delete file order indexes." << std::endl;
        file.close();
        delete[] completed_orders;
        completed_orders = NULL;
        return NULL;
    }

    double remaining_coins = order.fmi_coins;
    const size_t file_size = get_file_size(file);
    const size_t fmi_coins_seek = ORDER_SIZE - sizeof(double);

    for(size_t i = 0; i < file_size; i += ORDER_SIZE) {
        Order cur_order;
        load_order(file, cur_order);
        if(!file) {
            std::cerr << "Failed to load order from " << ORDER_FILENAME << " file." << std::endl;
            file.close();
            delete[] completed_orders;
            completed_orders = NULL;
            delete[] delete_indexes;
            delete_indexes = NULL;
            return NULL;
        }

        if(order.type == cur_order.type)
            continue;

        delete_indexes = ensure_indexes_size(delete_indexes, di_size, di_capacity, 1);
        if(!delete_indexes) {
            std::cerr << "Failed to resize delete indexes array due to dynamic memory issue." << std::endl;
            delete[] completed_orders;
            completed_orders = NULL;
            file.close();
            return NULL;
        }

        Order completed_order = cur_order;
        if(double_cmp(remaining_coins, cur_order.fmi_coins) < 0) {
            completed_order.fmi_coins = order.fmi_coins;
            cur_order.fmi_coins -= order.fmi_coins;
            remaining_coins = 0.000;
        } else if(double_cmp(remaining_coins, cur_order.fmi_coins) > 0) {
            remaining_coins -= cur_order.fmi_coins;
        } else {
            remaining_coins = 0.000;
        }

        completed_orders = add_order_to_completed(completed_orders, co_size, co_capacity, completed_order);
        if(!completed_orders) {
            std::cerr << "Failed to resize completed orders array due to dynamic memory issue." << std::endl;
            delete[] delete_indexes;
            delete_indexes = NULL;
            file.close();
            return NULL;
        }

        delete_indexes[di_size++] = i;

        if(remaining_coins < EPSILON) {
            if(delete_orders_at_positions(delete_indexes, di_size, file, file_size)) {
                if(double_cmp(completed_order.fmi_coins, cur_order.fmi_coins) != 0)
                    add_order_to_cache(cur_order);
                delete[] delete_indexes;
                delete_indexes = NULL;
                result_arr_size = co_size;
                order.fmi_coins = remaining_coins;
                return completed_orders;
            } else {
                delete[] delete_indexes;
                delete_indexes = NULL;
                delete[] completed_orders;
                completed_orders = NULL;
                std::cerr << "Failed to delete orders from " << ORDER_FILENAME << " file." << std::endl;
                return NULL;
            }
        }
    }

    if(delete_orders_at_positions(delete_indexes, di_size, file, file_size)) {
        delete[] delete_indexes;
        delete_indexes = NULL;
    } else {
        delete[] delete_indexes;
        delete_indexes = NULL;
        delete[] completed_orders;
        completed_orders = NULL;
        std::cerr << "Failed to delete orders from " << ORDER_FILENAME << " file." << std::endl;
        return NULL;
    }

    for(size_t i = 0; i < cache_size; ++i) {
        Order cur_order = cache[i];
        if(order.type == cur_order.type)
            continue;

        Order completed_order = cur_order;
        if(double_cmp(remaining_coins, cur_order.fmi_coins) < 0) {
            completed_order.fmi_coins = order.fmi_coins;
            cur_order.fmi_coins -= order.fmi_coins;
            add_order_to_cache(cur_order);
            remaining_coins = 0.000;
        } else if(double_cmp(remaining_coins, cur_order.fmi_coins) > 0) {
            remaining_coins -= cur_order.fmi_coins;
        } else {
            remaining_coins = 0.000;
        }
        delete_order_cache(i--);

        completed_orders = add_order_to_completed(completed_orders, co_size, co_capacity, completed_order);
        if(!completed_orders) {
            std::cerr << "Failed to resize completed orders array due to dynamic memory issue." << std::endl;
            return NULL;
        }

        if(remaining_coins < EPSILON) {
            result_arr_size = co_size;
            order.fmi_coins = remaining_coins;
            return completed_orders;
        }
    }

    result_arr_size = co_size;
    order.fmi_coins = remaining_coins;
    return completed_orders;
}
