#ifndef __UTIL_HEADER_INCLUDED__
#define __UTIL_HEADER_INCLUDED

size_t get_file_size(std::istream&);
int binary_search(const int* const, const size_t, const int);
int binary_search(const unsigned* const, const size_t, const unsigned);
int binary_search(const size_t* const, const size_t, const size_t);
int max(int, int);

#endif
