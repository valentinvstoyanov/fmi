#include <stdexcept>
#include <cstring>
#include <iostream>
#include "filemanager.h"

FileManager::FileManager(const char* name) {
    if(!name)
        throw std::invalid_argument("Null pointer passed as argument to FileManager constructor.");
    filename = new char[strlen(name) + 1];
    strcpy(filename, name);
}

FileManager::~FileManager() {
    delete[] filename;
    filename = nullptr;
}

void FileManager::write_lines(const Text& text) {
    std::ofstream file(filename);
    if(file) {
        text.print(file);
        file.close();
    } else {
        std::cerr << "Failed to open " << filename << " file." << std::endl;
    }
}

void FileManager::read_lines(Text& text) {
    std::ifstream file(filename);
    if(file) {
        const unsigned longest_line = 1024;
        char buff[longest_line];
        while(file.getline(buff, longest_line)) {
            Line line(buff);
            text.append_line(line);
        }
        file.close();
    } else {
        std::cerr << "Failed to open " << filename << " file." << std::endl;
    }
}
