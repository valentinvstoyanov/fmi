#include <iostream>
#include "ds/mystring.h"
#include "ds/pair.h"
#include "parser/json_parser.h"

int main() {
  JsonParser json_parser;
  
  String json("true");
  JsonValue* parsed_value = json_parser.Parse(json);
  if (JsonString* parsed_string = dynamic_cast<JsonString*>(parsed_value))
    std::cout << *parsed_string << std::endl;
  else if (JsonBoolean* parsed_bool = dynamic_cast<JsonBoolean*>(parsed_value))
    std::cout << *parsed_bool << std::endl;
  else
    std::cout << "Fail." << std::endl;

  delete parsed_value;

  return 0;
}