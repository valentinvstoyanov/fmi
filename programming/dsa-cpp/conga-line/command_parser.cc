//
// Created by valio_stoyanov on 11/5/18.
//

#include <iostream>
#include "command_parser.h"

void CommandParserAndHandler::OnAppend() {
  std::string name, university;
  unsigned conga_line_index;
  std::cin >> name >> university >> conga_line_index;
  Student student(name, university);
  long index = conga_collection_.Append(student, conga_line_index);
  if (index >= 0)
    std::cout << student << " appended to conga line with index " << index << std::endl;
  else
    std::cout << "Incompatible people" << std::endl;
}

void CommandParserAndHandler::OnRemoveFirst() {
  unsigned conga_line_index;
  std::cin >> conga_line_index;
  if (conga_line_index >= conga_collection_.Size()) {
    std::cout << "Conga line index " << conga_line_index << " is out of bounds" << std::endl;
    return;
  }
  conga_collection_.RemoveFirst(conga_line_index);
  std::cout << "Removed first student from conga line with index " << conga_line_index << std::endl;
}

void CommandParserAndHandler::OnRemoveLast() {
  unsigned conga_line_index;
  std::cin >> conga_line_index;
  if (conga_line_index >= conga_collection_.Size()) {
    std::cout << "Conga line index " << conga_line_index << " is out of bounds" << std::endl;
    return;
  }
  conga_collection_.RemoveLast(conga_line_index);
  std::cout << "Removed last student from conga line with index " << conga_line_index << std::endl;
}

void CommandParserAndHandler::OnRemoveByName() {
  std::string name;
  unsigned conga_line_index;
  std::cin >> name >> conga_line_index;
  if (conga_line_index >= conga_collection_.Size()) {
    std::cout << "Conga line index " << conga_line_index << " is out of bounds" << std::endl;
    return;
  }
  try {
    conga_collection_.Remove(name, conga_line_index);
  } catch (const CongaLine::StudentNotFoundException& e) {
    std::cout << name << " not found in conga line with index " << conga_line_index << std::endl;
    return;
  }
  std::cout << name << " has been removed from conga line with index "
            << conga_line_index << '.' << std::endl;
}

void CommandParserAndHandler::OnMerge() {
  unsigned conga_line_index1, conga_line_index2;
  std::cin >> conga_line_index1 >> conga_line_index2;
  if (conga_line_index1 >= conga_collection_.Size()
      || conga_line_index2 >= conga_collection_.Size()) {
    std::cout << "Line indices " << conga_line_index1 << " and "
              << conga_line_index2 << " out of bounds." << std::endl;
    return;
  }
  if (conga_collection_.Merge(conga_line_index1, conga_line_index2))
    std::cout << "Conga lines with indices "
              << conga_line_index1 << " and " << conga_line_index2
              << " are merged." << std::endl;
  else
    std::cout << "Incompatible people" << std::endl;
}

void CommandParserAndHandler::OnPrint() const {
  std::cout << conga_collection_ << std::endl;
}

CommandParserAndHandler::CommandParserAndHandler(const CongaCollection& collection)
    : conga_collection_(collection) {}

void CommandParserAndHandler::ParseAndHandle() {
  std::string command;
  while (true) {
    std::cout << "$: ";
    std::cin >> command;
    if (command == "quit") {
      std::cout << "Program closed" << std::endl;
      break;
    }
    if (conga_collection_.Empty()) {
      if (!command.compare(0, 5, "print")) OnPrint();
      std::cout << "Conga collection is empty, so you can use 'print' and 'quit' commands only" << std::endl;
      continue;
    }
    if (!command.compare(0, 6, "append")) OnAppend();
    else if (!command.compare(0, 10, "removeLast")) OnRemoveLast();
    else if (!command.compare(0, 11, "removeFirst")) OnRemoveFirst();
    else if (!command.compare(0, 6, "remove")) OnRemoveByName();
    else if (!command.compare(0, 5, "merge")) OnMerge();
    else if (!command.compare(0, 5, "print")) OnPrint();
    else std::cout << "Unknown command" << std::endl;
  }
}
