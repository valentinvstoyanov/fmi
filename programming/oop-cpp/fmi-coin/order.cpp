#include <iostream>
#include <fstream>
#include <cstring>
#include <unistd.h>
#include <cmath>
#include "order.h"
#include "file.h"

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

int search(const size_t* arr, const size_t size, const size_t element) {
	unsigned left = 0;
	unsigned right = size - 1;

	while(left <= right) {
		unsigned middle = (left + right) / 2;

		if(arr[middle] == element)
			return middle;

		if(element < arr[middle])
			right = middle - 1;
		else
			left = middle + 1;
	}

	return -1;
}

bool contains(const size_t* arr, const size_t size, const size_t element) {
    return search(arr, size, element) >= 0;
}

bool delete_orders_at_positions(const size_t* positions, const size_t pos_size, std::fstream& file, const size_t file_size) {
    const char* tmp_filename = "ORDERS_TMP.dat";
    std::ofstream new_file;
    new_file.open(tmp_filename, std::ios::binary | std::ios::out);
    file.seekg(0, std::ios::beg);
    const size_t order_size = sizeof(Order::Type) + sizeof(unsigned) + sizeof(double);
    for(size_t current_pos = 0; file && new_file && current_pos < file_size; current_pos += order_size) {
        if(contains(positions, pos_size, current_pos)) {
            Order tmp;
            load_order(file, tmp);
            save_order(new_file, tmp);
        } else {
            file.seekg(order_size, std::ios::cur);
        }
    }
    file.close();
    new_file.close();
    return file && new_file && !unlink(ORDER_FILENAME) && !rename(tmp_filename, ORDER_FILENAME);
}

int max(int a, int b) {
    return a < b ? b : a;
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

size_t* ensure_pos_size(size_t* arr, const size_t size, size_t& capacity, size_t new_elements) {
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

Order* complete_orders(Order& order, int& res_size) {
    size_t co_capacity = 4;
    size_t co_size = 0;
    Order* completed_orders = new(std::nothrow) Order[co_capacity];
    if(!completed_orders)
        return NULL;

    size_t rm_capacity = 2;
    size_t rm_size = 0;
    size_t* remove_positions = new(std::nothrow) size_t[rm_capacity];
    if(!remove_positions) {
        delete[] completed_orders;
        return NULL;
    }

    double remaining_coins = order.fmi_coins;

    std::fstream file(ORDER_FILENAME, std::ios::binary | std::ios::in | std::ios::app);
    if(!file) {
        delete[] completed_orders;
        delete[] remove_positions;
        return NULL;
    }
    const size_t file_size = get_file_size(file);
    const size_t order_size = sizeof(unsigned) + sizeof(double) + sizeof(Order::Type);
    const size_t seek_to_coins_size = sizeof(Order::Type) + sizeof(unsigned);
    const double epsilon = 0.001;
    for(size_t i = 0; i < file_size; i += order_size) {
        Order tmp;
        load_order(file, tmp);
        if(tmp.type != order.type) {
            if(fabs(order.fmi_coins - tmp.fmi_coins) < epsilon) {
                if((completed_orders = ensure_orders_size(completed_orders, co_size, co_capacity, 2))
                && (remove_positions = ensure_pos_size(remove_positions, rm_size, rm_capacity, 1))) {
                    completed_orders[co_size++] = order;
                    completed_orders[co_size++] = tmp;
                    remove_positions[rm_size++] = i;
                    remaining_coins = 0;
                }
            } else if(order.fmi_coins + epsilon > tmp.fmi_coins) {
                if((completed_orders = ensure_orders_size(completed_orders, co_size, co_capacity, 1))
                && (remove_positions = ensure_pos_size(remove_positions, rm_size, rm_capacity, 1))) {
                    completed_orders[co_size++] = tmp;
                    remove_positions[rm_size++] = i;
                    remaining_coins -= tmp.fmi_coins;
                }
            } else {
                if(completed_orders = ensure_orders_size(completed_orders, co_size, co_capacity, 2)) {
                    completed_orders[co_size++] = order;
                    const double tmp_new_coins = tmp.fmi_coins - order.fmi_coins;
                    tmp.fmi_coins = order.fmi_coins;
                    completed_orders[co_size++] = tmp;
                    tmp.fmi_coins = tmp_new_coins;
                    add_order_to_cache(tmp);
                    remaining_coins = 0;
                }
            }

            if(!(completed_orders && remove_positions) || !file) {
                delete[] completed_orders;
                delete[] remove_positions;
                file.close();
                std::cerr << "Problem with " << ORDER_FILENAME << " file or with dynamic memory allocation while completing orders." << std::endl;
                return NULL;
            }

            if(remaining_coins < epsilon)
                break;
        }
    }

    if(!delete_orders_at_positions(remove_positions, rm_size, file, file_size)) {
        std::cerr << "Failed to delete orders from " << ORDER_FILENAME << " file!" << std::endl;
        delete[] completed_orders;
        delete[] remove_positions;
        return NULL;
    }

    delete[] remove_positions;
    res_size = co_size;
    order.fmi_coins = remaining_coins;
    if(remaining_coins < epsilon)
        return completed_orders;

    for (unsigned short i = 0; i < cache_size; ++i) {
        Order tmp = cache[i];
        if(tmp.type != order.type) {
            if(fabs(order.fmi_coins - tmp.fmi_coins) < epsilon) {
                if(completed_orders = ensure_orders_size(completed_orders, co_size, co_capacity, 2)) {
                    completed_orders[co_size++] = order;
                    completed_orders[co_size++] = tmp;
                    delete_order_cache(i);
                    remaining_coins = 0;
                }
            } else if(order.fmi_coins + epsilon > tmp.fmi_coins) {
                if(completed_orders = ensure_orders_size(completed_orders, co_size, co_capacity, 1)) {
                    completed_orders[co_size++] = tmp;
                    delete_order_cache(i);
                    remaining_coins -= tmp.fmi_coins;
                }
            } else {
                if(completed_orders = ensure_orders_size(completed_orders, co_size, co_capacity, 2)) {
                    completed_orders[co_size++] = order;
                    const double tmp_new_coins = tmp.fmi_coins - order.fmi_coins;
                    tmp.fmi_coins = order.fmi_coins;
                    completed_orders[co_size++] = tmp;
                    tmp.fmi_coins = tmp_new_coins;
                    add_order_to_cache(tmp);
                    remaining_coins = 0;
                }
            }

            if(!completed_orders) {
                delete[] completed_orders;
                std::cerr << "Problem with dynamic memory allocation while completing orders." << std::endl;
                return NULL;
            }

            if(remaining_coins < epsilon)
                break;
        }
    }

    res_size = co_size;
    order.fmi_coins = remaining_coins;
    return completed_orders;
}
