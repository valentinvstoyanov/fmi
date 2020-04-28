//
// Created by valio_stoyanov on 11/3/18.
//

#include "command_parser.h"

int main() {
  CongaLine conga_line;
  conga_line.PushBack(Student("Integralcho", "fmi"));

  CongaCollection conga_collection;
  conga_collection.PushBack(conga_line);

  CommandParserAndHandler command_parser_and_handler(conga_collection);
  command_parser_and_handler.ParseAndHandle();

  return 0;
}
