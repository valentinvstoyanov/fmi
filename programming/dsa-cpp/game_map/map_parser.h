#ifndef GAME_MAP_MAP_PARSER_H
#define GAME_MAP_MAP_PARSER_H

#include <fstream>
#include <vector>
#include "game_map.h"

class MapParser {
  void readZone(const std::string&, Graph<std::string, std::string>&) const;
  void readKey(const std::string&,
               std::unordered_map<std::string, std::vector<std::string>>&) const;
 public:
  bool parseFrom(const char*,
                 Graph<std::string, std::string>&,
                 std::unordered_map<std::string, std::vector<std::string>>&) const;
  bool parseTo(const char*, const GameMap&) const;
};

#endif //GAME_MAP_MAP_PARSER_H
