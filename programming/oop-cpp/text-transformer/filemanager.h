#ifndef __FILEMANAGER_HEADER_INCLUDED__
#define __FILEMANAGER_HEADER_INCLUDED__
#include <fstream>
#include "text.h"

class FileManager {
    char* filename;
public:
    FileManager(const char*);
    FileManager(const FileManager&) = delete;
    FileManager& operator=(const FileManager&) = delete;
    ~FileManager();
    void write_lines(const Text&);
    void read_lines(Text&);
};

#endif
