//
// Created by valio_stoyanov on 11/3/18.
//

#include <iostream>
#include "conga_collection.h"

void ReadCommand(std::string& command) {
  std::cout << "$: ";
  std::cin >> command;
}

void OnQuit() {
  std::cout << "Program closed." << std::endl;
}

void OnAppend(CongaCollection& collection) {
  std::string name;
  std::cin >> name;
  std::string uni;
  std::cin >> uni;
  unsigned line_index;
  std::cin >> line_index;
  try {
    Student student(name, uni);
    long index = collection.Append(student, line_index);
    if (index >= 0)
      std::cout << student << " appended to conga line with index "
                << index << std::endl;
    else
      std::cout << "Failed to append student: " << student
                << " to conga line with index " << line_index
                << '.' << std::endl;
  } catch (const Student::NoSuchUniException& e) {
    std::cout << e.what() << std::endl;
  }
}

void OnRemoveInEnds(CongaCollection& collection, bool last) {
  unsigned line_index;
  std::cin >> line_index;
  if (line_index >= collection.Size()) {
    std::cout << "Line index out of bounds." << std::endl;
    return;
  }
  last ? collection.RemoveLast(line_index) : collection.RemoveFirst(line_index);
  std::cout << "Removed " << (last ? "last" : "first")
            << " student from conga line with index " << line_index
            << '.' << std::endl;
}

void OnRemove(CongaCollection& collection) {
  std::string name;
  std::cin >> name;
  unsigned line_index;
  std::cin >> line_index;
  if (line_index >= collection.Size()) {
    std::cout << "Line index out of bounds." << std::endl;
    return;
  }
  try {
    collection.Remove(name, line_index);
  } catch (const CongaLine::StudentNotFoundException& e) {
    std::cout << e.what() << " in conga line with index "
              << line_index << '.' << std::endl;
  }
  std::cout << name << " has been removed from conga line with index "
            << line_index << '.' << std::endl;
}

void OnMerge(CongaCollection& collection) {
  unsigned line_index1, line_index2;
  std::cin >> line_index1 >> line_index2;
  if (line_index1 >= collection.Size() || line_index2 >= collection.Size()) {
    std::cout << "Line indices out of bounds." << std::endl;
    return;
  }
  collection.Merge(line_index1, line_index2);
  std::cout << "Conga lines with indices "
            << line_index1 << " and " << line_index2
            << " are merged." << std::endl;
}

void OnPrint(CongaCollection& collection) {
  std::cout << collection << std::endl;
}

void OnInvalidCommand() {
  std::cout << "No such command. Please try again with another one."
            << std::endl;
}

int main() {
  CongaCollection collection;
  std::string command;
  while (true) {
    ReadCommand(command);
    if (command == "quit") {
      OnQuit();
      break;
    }
    if (!command.compare(0, 6, "append"))
      OnAppend(collection);
    else if (!command.compare(0, 10, "removeLast"))
      OnRemoveInEnds(collection, true);
    else if (!command.compare(0, 11, "removeFirst"))
      OnRemoveInEnds(collection, false);
    else if (!command.compare(0, 6, "remove"))
      OnRemove(collection);
    else if (!command.compare(0, 5, "merge"))
      OnMerge(collection);
    else if (!command.compare(0, 5, "print"))
      OnPrint(collection);
    else
      OnInvalidCommand();

  }
  return 0;
}
