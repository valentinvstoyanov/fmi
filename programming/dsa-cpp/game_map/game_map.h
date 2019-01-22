#ifndef GAME_MAP_GAME_MAP_H
#define GAME_MAP_GAME_MAP_H

#include <vector>
#include <ostream>
#include "graph.h"

class GameMap {
  using KeyMap = std::unordered_map<std::string, std::vector<std::string>>;

  std::string initial_zone;
  KeyMap keys;
  Graph<std::string, std::string> zones;

  bool canPass(const std::string&, const std::unordered_set<std::string>&) const;
  void collectKeysFromZone(const std::string&, std::unordered_set<std::string>&) const;
  void collectAllKeysFromZones(std::unordered_set<std::string>&) const;

 public:
  GameMap(const std::string&, const Graph<std::string, std::string>&, const KeyMap&);
  KeyMap& getKeys();
  Graph<std::string, std::string>& getZones();
  friend std::ostream& operator<<(std::ostream&, const GameMap&);
};

#endif //GAME_MAP_GAME_MAP_H
