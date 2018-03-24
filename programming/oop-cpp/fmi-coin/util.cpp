#include <fstream>
#include "util.h"

size_t get_file_size(std::istream& f) {
    size_t current_pos = f.tellg();
    f.seekg(0, std::ios::end);
    size_t size = f.tellg();
    f.seekg(current_pos, std::ios::beg);
    return size;
}

int binary_search(const int* const arr, const size_t size, const int element) {
    size_t left = 0;
    size_t right = size - 1;

    while(left <= right) {
        size_t middle = (left + right) / 2;

        if(arr[middle] == element)
            return middle;

        if(element < arr[middle])
            right = middle - 1;
        else
            left = middle + 1;
    }

    return -1;
}

int binary_search(const unsigned* const arr, const size_t size, const unsigned element) {
    size_t left = 0;
    size_t right = size - 1;

    while(left <= right) {
        size_t middle = (left + right) / 2;

        if(arr[middle] == element)
            return middle;

        if(element < arr[middle])
            right = middle - 1;
        else
            left = middle + 1;
    }

    return -1;
}

int binary_search(const size_t* const arr, const size_t size, const size_t element) {
    size_t left = 0;
    size_t right = size - 1;

    while(left <= right) {
        size_t middle = (left + right) / 2;

        if(arr[middle] == element)
            return middle;

        if(element < arr[middle])
            right = middle - 1;
        else
            left = middle + 1;
    }

    return -1;
}

int max(int a, int b) {
    return a < b ? b : a;
}
