#include "map_parser.h"

void MapParser::readZone(const std::string& line,
                         Graph<std::string, std::string>& graph) const {
  const unsigned long first_word_end_idx = line.find(' ');
  const unsigned long second_word_start_idx = first_word_end_idx + 4;
  const unsigned long lopsqi = line.find_last_of('[');
  const unsigned long last_open_sq_bracket_idx = lopsqi == std::string::npos ? line.length() : lopsqi;
  std::string from = line.substr(0, first_word_end_idx);
  std::string to = line.substr(second_word_start_idx, last_open_sq_bracket_idx - second_word_start_idx - 1);
  std::string edge = lopsqi == std::string::npos ? "" : line.substr(last_open_sq_bracket_idx + 1,
                                                                    line.length() - last_open_sq_bracket_idx - 3);
  graph.addVertex(from);
  graph.addVertex(to);
  graph.addEdge(from, to, edge);
}

void MapParser::readKey(const std::string& line,
                        std::unordered_map<std::string, std::vector<std::string>>& keys) const {
  const unsigned long first_dash_idx = line.find('-');
  std::string key = line.substr(0, first_dash_idx - 1);
  std::string zone = line.substr(first_dash_idx + 3, line.length() - first_dash_idx - 4);
  keys[zone].push_back(key);
}

bool MapParser::parseFrom(const char* filename,
                          Graph<std::string, std::string>& zones,
                          std::unordered_map<std::string, std::vector<std::string>>& keys) const {
  std::ifstream file(filename);
  if (!file.is_open())
    return false;

  std::string line;
  std::getline(file, line);
  std::getline(file, line);
  while (line != "[keys]") {
    if (!line.empty())
      readZone(line, zones);
    std::getline(file, line);
  }

  while (std::getline(file, line)) {
    if (!line.empty())
      readKey(line, keys);
  }

  return true;
}

bool MapParser::parseTo(const char* filename, const GameMap& game_map) const {
  std::ofstream file(filename);
  if (!file.is_open())
    return false;

  file << game_map;
  return true;
}
