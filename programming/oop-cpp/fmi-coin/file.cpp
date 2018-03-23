#include <fstream>
#include "file.h"

size_t get_file_size(std::ifstream& f) {
    size_t current_pos = f.tellg();
    f.seekg(0, std::ios::end);
    size_t size = f.tellg();
    f.seekg(current_pos, std::ios::beg);
    return size;
}

size_t get_file_size(std::fstream& f) {
    size_t current_pos = f.tellg();
    f.seekg(0, std::ios::end);
    size_t size = f.tellg();
    f.seekg(current_pos, std::ios::beg);
    return size;
}
