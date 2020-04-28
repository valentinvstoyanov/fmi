//
// Created by valio_stoyanov on 12/14/18.
//

#ifndef CHORBADJIISKO_HORO_PARSER_H
#define CHORBADJIISKO_HORO_PARSER_H

#include <vector>
#include "handler.h"

class Parser {
  Handler handler_;

  std::string& RemoveQuotes(std::string&) const;
  void Tokenize(const std::string&, std::vector<std::string>&, char) const;
  bool ValidateParametersCount(const std::vector<std::string>&, unsigned) const;
 public:
  explicit Parser(const Handler&);

  void Start();

};

#endif //CHORBADJIISKO_HORO_PARSER_H
