//
// Created by valio_stoyanov on 12/14/18.
//

#include <iostream>
#include "parser.h"

Parser::Parser(const Handler& handler) : handler_(handler) {}

void Parser::Start() {
  std::cout << "Starting..." << std::endl;

  std::string line;
  while (getline(std::cin, line)) {

    if (line == "EXIT") {
      handler_.Exit();
      break;
    }

    if (line == "PRINT") {
      handler_.Print();
      continue;
    }

    std::vector<std::string> strs;

    Tokenize(line, strs, ' ');

    if (strs.empty()) {
      handler_.InputErr("invalid command.");
      continue;
    }

    if (strs[0] == "RELEASE") {
      if (!ValidateParametersCount(strs, 3))
        continue;

      Hold hold;
      if (strs[2] == "left") {
        hold = kLeft;
      } else if (strs[2] == "right") {
        hold = kRight;
      } else if (strs[2] == "both") {
        hold = kBoth;
      } else {
        handler_.InputErr("left, right or both expected");
        continue;
      }

      handler_.Release(RemoveQuotes(strs[1]), hold);
    } else if (strs[0] == "GRAB") {
      if (!ValidateParametersCount(strs, 3))
        continue;

      Hold hold;
      if (strs[2] == "left") {
        hold = kLeft;
      } else if (strs[2] == "right") {
        hold = kRight;
      } else if (strs[2] == "both") {
        hold = kBoth;
      } else {
        handler_.InputErr("left, right or both expected");
        continue;
      }

      handler_.Grab(RemoveQuotes(strs[1]), hold);
    } else if (strs[0] == "INFO") {
      if (!ValidateParametersCount(strs, 2))
        continue;

      handler_.Info(strs[1]);
    } else if (strs[0] == "ADD") {
      if (!ValidateParametersCount(strs, 4))
        continue;

      handler_.Add(RemoveQuotes(strs[1]), RemoveQuotes(strs[2]), RemoveQuotes(strs[3]));
    } else if (strs[0] == "REMOVE") {
      if (!ValidateParametersCount(strs, 2))
        continue;

      handler_.Remove(strs[1]);
    } else if (strs[0] == "SWAP") {
      if (!ValidateParametersCount(strs, 3))
        continue;

      handler_.Swap(RemoveQuotes(strs[1]), RemoveQuotes(strs[2]));
    } else {
      handler_.InputErr("unknown command.");
    }
  }
}

void Parser::Tokenize(const std::string& text, std::vector<std::string>& res, char delim) const {
  size_t pos = text.find(delim);
  size_t initial_pos = 0;

  while (pos != std::string::npos) {
    res.push_back(text.substr(initial_pos, pos - initial_pos));
    initial_pos = pos + 1;
    pos = text.find(delim, initial_pos);
  }

  res.push_back(text.substr(initial_pos, std::min(pos, text.size()) - initial_pos + 1));
}

std::string& Parser::RemoveQuotes(std::string& str) const {
  return str = str.substr(1, str.size() - 2);
}

bool Parser::ValidateParametersCount(const std::vector<std::string>& strs,
                                     unsigned expected_count) const {
  if (strs.size() != expected_count) {
    handler_.InputErr(std::to_string(expected_count) + " parameters expected.");
    return false;
  }

  return true;
}
