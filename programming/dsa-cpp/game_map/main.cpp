#include <iostream>
#include "map_parser.h"


int main(int argc, char** argv) {
  const char* filename = argv[1];
  const char* initial_zone = argv[2];
  const char* output_filename = "out.dot";

  Graph<std::string, std::string> zones;
  std::unordered_map<std::string, std::vector<std::string>> keys;
  MapParser map_parser;
  if (map_parser.parseFrom(filename, zones, keys)) {
    GameMap game_map(initial_zone, zones, keys);
    std::cout << "Parsing file " << filename << " succeeded!" << std::endl;

    if(map_parser.parseTo(output_filename, game_map))
      std::cout << "Output file generation succeeded!" << std::endl;
    else
      std::cout << "Failed to generate output file." << std::endl;
  } else {
    std::cout << "Failed to parse file " << filename << '.' << std::endl;
  }

  return 0;
}