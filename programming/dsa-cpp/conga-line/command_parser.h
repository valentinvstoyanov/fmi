//
// Created by valio_stoyanov on 11/5/18.
//

#ifndef CONGA_LINE_COMMAND_PARSER_H
#define CONGA_LINE_COMMAND_PARSER_H

#include "conga_collection.h"
class CommandParserAndHandler {
  void OnAppend();
  void OnRemoveFirst();
  void OnRemoveLast();
  void OnRemoveByName();
  void OnMerge();
  void OnPrint() const;
  CongaCollection conga_collection_;
 public:
  explicit CommandParserAndHandler(const CongaCollection&);
  void ParseAndHandle();
};

#endif //CONGA_LINE_COMMAND_PARSER_H
