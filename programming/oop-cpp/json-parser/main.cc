#include <iostream>
#include "ds/mystring.h"
#include "ds/pair.h"
#include "parser/json_parser.h"
#include "util/cstr.h"
#include "model/json_value.h"
#include "writer/json_writer.h"

const unsigned MAX_CMD_LEN = 256;

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
    JsonParser json_parser;
    JsonValue* value = json_parser.Parse(json);
    JsonWriter writer;
    writer.SetPretty(false);
    writer.Write(std::cout, *value);
    delete value;
  } catch (const JsonParser::JsonParseException& e) {
    std::cerr << "Exception catched..." << std::endl;
  }

  /*
  bool running = true;
  do {

  } while (running);
   */

  return 0;
}