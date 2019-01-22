#include "game_map.h"

GameMap::GameMap(const std::string& str,
                 const Graph<std::string, std::string>& graph,
                 const GameMap::KeyMap& map)
    : initial_zone(str), zones(graph), keys(map) {}

GameMap::KeyMap& GameMap::getKeys() {
  return keys;
}

Graph<std::string, std::string>& GameMap::getZones() {
  return zones;
}

bool GameMap::canPass(const std::string& edge, const std::unordered_set<std::string>& keys) const {
  return edge.empty() || keys.find(edge) != keys.end();
}

void GameMap::collectKeysFromZone(const std::string& zone, std::unordered_set<std::string>& result) const {
  auto keys_it = keys.find(zone);
  if (keys_it != keys.end())
    result.insert(keys_it->second.begin(), keys_it->second.end());
}

void GameMap::collectAllKeysFromZones(std::unordered_set<std::string>& result) const {
  std::unordered_map<std::string, std::unordered_map<std::string, std::string>> not_visited;
  auto collector = [this, &result, &not_visited](const std::string& from1, const std::string& edge1, const std::string& to1) -> bool {
    collectKeysFromZone(from1, result);

    if (!canPass(edge1, result)) {
      bool can_return_back = false;
      std::unordered_set<std::string> temp_keys;

      zones.BFS(from1,
                [this, &temp_keys, &result, &from1, &can_return_back, &not_visited](const std::string& from2,
                                                                      const std::string& edge2,
                                                                      const std::string& to2) -> bool {
                  if (canPass(edge2, result)) {
                    collectKeysFromZone(to2, temp_keys);

                    zones.BFS(to2,
                              [this, &result, &from1, &can_return_back, &not_visited](const std::string& from3,
                                                                        const std::string& edge3,
                                                                        const std::string& to3) -> bool {
                                if (to3 == from1 && canPass(edge3, result)) {
                                  can_return_back = true;
                                  return false;
                                }

                                return true;
                              }, false, not_visited);

                    return false;
                  }

                  return true;
                }, false, not_visited);

      if (can_return_back)
        result.insert(temp_keys.begin(), temp_keys.end());

      return canPass(edge1, result);
    }

    return true;
  };

  zones.BFS(initial_zone, collector, false, not_visited);
}

std::ostream& operator<<(std::ostream& out, const GameMap& game_map) {
  std::unordered_set<std::string> collected_keys;
  game_map.collectAllKeysFromZones(collected_keys);

  out << "digraph {\n";

  std::unordered_map<std::string, std::unordered_map<std::string, std::string>> not_visited;

  game_map.zones.BFS(game_map.initial_zone,
                     [&game_map, &collected_keys, &out](const std::string& from,
                                                        const std::string& edge,
                                                        const std::string& to) -> bool {
                       out << from << " -> " << to;
                       (edge.empty() ? out << "" : out << "[label=\"" << edge << "\"]") << ';';
                       if (!game_map.canPass(edge, collected_keys))
                         out << to << "[color=red,style=filled,fillcolor=\"#ffefef\"];";

                       return true;
                     }, true, not_visited);

  for (auto v_it = not_visited.begin(); v_it != not_visited.end(); ++v_it) {
    for (auto e_it = v_it->second.begin(); e_it != v_it->second.end(); ++e_it) {
      out << v_it->first << " -> " << e_it->first;
      (e_it->second.empty() ? out << "" : out << "[label=\"" << e_it->second << "\"]") << ';';
      out << v_it->first << "[color=red,style=filled,fillcolor=\"#ffefef\"];";
    }
  }

  out << "}";

  return out;
}
