#ifndef __FILEMANAGER_HEADER_INCLUDED__
#define __FILEMANAGER_HEADER_INCLUDED__
#include "text.h"

class FileManager {
    char* filename;
public:
    FileManager(const char*);
    FileManager(const FileManager&);
    FileManager& operator=(const FileManager&);
    ~FileManager();
    bool write_lines(const Text&);
    bool read_lines(Text&);
};

#endif
