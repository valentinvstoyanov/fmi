#include <fstream>
#include <cmath>
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

int max(const int a, const int b) {
    return a < b ? b : a;
}

int double_cmp(const double a, const double b) {
    if(fabs(a - b) < EPSILON)
        return 0;
    if(a + EPSILON > b)
        return 1;
    else
        return -1;
}

int insert_sorted(unsigned* arr, const size_t size, const size_t capacity, const unsigned element) {
    if (size >= capacity)
       return -1;

    size_t i;
    for (i = size-1; i >= 0  && arr[i] > element; --i)
       arr[i + 1] = arr[i];

    arr[i + 1] = element;

    return i + 1;
}
