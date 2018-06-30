#include <iostream>
#include "ds/mystring.h"
#include "ds/pair.h"
#include "parser/json_parser.h"
#include "util/cstr.h"
#include "model/json_value.h"
#include "writer/json_writer.h"
#include "exception/deserialize_exception.h"

int main() {
  try {
    const char* json = "{\"menu\": {\r\n  \"id\": \"file\",\r\n  \"value\": \"File\",\r\n"
                       "  \"popup\": {\r\n    \"menuitem\": [\r\n      {\"value\": \"New\","
                       " \"onclick\": \"CreateNewDoc()\"},\r\n      {\"value\": \"Open\", "
                       "\"onclick\": \"OpenDoc()\"},\r\n      {\"value\": \"Close\", "
                       "\"onclick\": \"CloseDoc()\"}\r\n,{\"menu\": {\r\n  \"id\": \"file\",\r\n  \"value\": \"File\",\r\n"
                       "  \"popup\": {\r\n    \"menuitem\": [\r\n      {\"value\": \"New\","
                       " \"onclick\": \"CreateNewDoc()\"},\r\n      {\"value\": \"Open\", "
                       "\"onclick\": \"OpenDoc()\"},\r\n      {\"value\": \"Close\", "
                       "\"onclick\": \"CloseDoc()\"}\r\n   ]\r\n  }\r\n}}   ]\r\n  }\r\n}}";
    JsonValue* value = JsonParser::ParseFromFile("test.txt", true);
    JsonWriter writer;
    writer.SetPretty(true);
    writer.Write(std::cout, *value);

    JsonParser::WriteToFile(*value, "test_ouput_json.txt", true);

    delete value;
  } catch (const DeserializeException& e) {
    std::cerr << e.what() << std::endl;
  }

  /*
  bool running = true;
  do {

  } while (running);
   */

  return 0;
}