#include <iostream>
#include "model/json_value.h"
#include "parser/json_parser.h"
#include "exception/deserialize_exception.h"

int main() {
  try {
    JsonValue* value = JsonParser::ParseFromFile("test.txt", true);
    JsonParser::PrintToStdin(*value, false);
    JsonParser::WriteToFile(*value, "test_ouput_json.txt", false);
    delete value;
  } catch (const DeserializeException& e) {
    std::cerr << e.what() << std::endl;
  }

  return 0;
}