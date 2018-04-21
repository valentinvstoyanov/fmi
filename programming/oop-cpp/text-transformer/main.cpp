#include <iostream>
#include "formatter.h"
#include "filemanager.h"
#include <cstring>

bool get_text_from_file(const char* filename, Text& text) {
    FileManager in_file_manager(filename);
    return in_file_manager.read_lines(text);
}

bool save_text_to_file(const char* filename, const Text& text) {
    FileManager out_file_manager(filename);
    return out_file_manager.write_lines(text);
}

int main() {
    const char heading_cmd[] = "makeHeader";
    const char italic_cmd[] = "makeItalic";
    const char bold_cmd[] = "makeBold";
    const char combine_cmd[] = "makeCombine";
    const char add_cmd[] = "addLine";
    const char remove_cmd[] = "remove";
    const char exit_cmd[] = "exit";

    char filename[256];
    Text text;
    do {
        std::cout << "(Enter filename) -> ";
        std::cin.getline(filename, 256);
        if(!strcmp(filename, exit_cmd))
            return 0;
        if(get_text_from_file(filename, text))
            break;
        else
            std::cout << "(Info) -> Please try again." << std::endl;
    } while(true);

    text.print();

    Formatter formatter;
    const unsigned longest_cmd = 11;
    char cmd[longest_cmd];
    bool running = true;
    do {
        std::cout << "(Enter command) -> ";
        std::cin >> cmd;
        if(!strcmp(heading_cmd, cmd)) {
            unsigned line_number;
            std::cin >> line_number;
            text.set_line_at(formatter.make_heading(text.get_line_at(line_number - 1)), line_number - 1);
        } else if(!strcmp(italic_cmd, cmd)) {
            unsigned line_number, from, to;
            std::cin >> line_number >> from >> to;
            text.set_line_at(formatter.make_italic(text.get_line_at(line_number - 1), from, to), line_number - 1);
        } else if(!strcmp(bold_cmd, cmd)) {
            unsigned line_number, from, to;
            std::cin >> line_number >> from >> to;
            text.set_line_at(formatter.make_bold(text.get_line_at(line_number - 1), from, to), line_number - 1);
        } else if(!strcmp(combine_cmd, cmd)) {
            unsigned line_number, from, to;
            std::cin >> line_number >> from >> to;
            text.set_line_at(formatter.make_combined(text.get_line_at(line_number - 1), from, to), line_number - 1);
        } else if(!strcmp(add_cmd, cmd)) {
            char buff[1024];
            std::cin.ignore();
            std::cin.getline(buff, 1024);
            Line line(buff);
            text.append_line(line);
        } else if(!strcmp(remove_cmd, cmd)) {
            unsigned line_number;
            std::cin >> line_number;
            text.remove_line(line_number - 1);
        } else if(!strcmp(exit_cmd, cmd)) {
            Formatter extension_formatter(".");
            char* formatted_filename = extension_formatter.change_filename_extension(filename, "md");
            save_text_to_file(formatted_filename, text);
            delete[] formatted_filename;
            formatted_filename = nullptr;
            std::cout << "(Info) -> Text successfully transformed to MarkDown." << std::endl;
            running = false;
        } else {
            std::cout << "(Error) -> Invalid command." << std::endl;
        }
    } while(running);

    return 0;
}
