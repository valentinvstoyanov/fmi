#include <iostream>
#include "formatter.h"
#include "filemanager.h"
#include <cstring>

int main() {
    const char heading_cmd[] = "makeHeading";
    const char italic_cmd[] = "makeItalic";
    const char bold_cmd[] = "makeBold";
    const char combine_cmd[] = "makeCombine";
    const char add_cmd[] = "addLine";
    const char remove_cmd[] = "remove";
    const char exit_cmd[] = "exit";

    Text text;
    FileManager file_manager("TEST.txt");
    file_manager.read_lines(text);
    text.print();
    Formatter formatter;

    const unsigned longest_cmd = 11;
    char cmd[longest_cmd];
    do{
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
        FileManager write_file_manager("TEST.md");
        write_file_manager.write_lines(text);
        std::cout << "Text successfully transformed to MarkDown." << std::endl;
        break;
    } else {
        std::cout << "Invalid command." << std::endl;
    }
}while(true);

    return 0;
}
