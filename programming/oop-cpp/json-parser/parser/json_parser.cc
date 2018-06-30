#include <fstream>
#include "json_parser.h"
#include "../util/cstr.h"
#include "../util/json_token.h"
#include "../model/json_object.h"
#include "../exception/deserialize_exception.h"

void JsonParser::PrintError(const std::runtime_error& error) {
  std::cerr << error.what() << std::endl;
}

JsonValue* JsonParser::Validate(const char* json) {
  try {
    return JsonValue::FromJson(json);
  } catch (const DeserializeException& error) {
    PrintError(error);
    return nullptr;
  }
}

bool JsonParser::CheckValidity(const char* json) {
  return Validate(json) != nullptr;
}

bool JsonParser::CheckValidity(const String& json) {
  return CheckValidity(json.CStr());
}

JsonValue* JsonParser::ParseFromFile(const File& file, bool nothrow) {
  if (nothrow) {
    try {
      return ParseFromString(file.GetContent(), nothrow);
    } catch (const File::FileException& error) {
      PrintError(error);
      return nullptr;
    }
  }else {
    return ParseFromString(file.GetContent(), nothrow);
  }
}

JsonValue* JsonParser::ParseFromFile(const char* filename, bool nothrow) {
  return ParseFromFile(String(filename), nothrow);
}
JsonValue* JsonParser::ParseFromFile(const String& filename, bool nothrow) {
  return ParseFromFile(File(filename), nothrow);
}

JsonValue* JsonParser::ParseFromString(const char* json, bool nothrow) {
  return nothrow ? Validate(json) : JsonValue::FromJson(json);
}

JsonValue* JsonParser::ParseFromString(const String& filename, bool nothrow) {
  return ParseFromString(filename.CStr(), nothrow);
}

void JsonParser::WriteToFile(const JsonValue& json,
                             const char* filename,
                             bool pretty) {
  WriteToFile(json, File(filename), pretty);
}

void JsonParser::WriteToFile(const JsonValue& json,
                             const String& filename,
                             bool pretty) {
  WriteToFile(json, File(filename), pretty);
}

void JsonParser::WriteToFile(const JsonValue& json,
                             const File& file,
                             bool pretty) {
  if (file.Exists()) {
    std::cout << "If you want to override file content enter Y or y. "
                 "N or n to cancel: ";
    char c;
    do {
      std::cin >> c;
      if (c == 'Y' || c == 'y') {
        file.Save(json, pretty);
        std::cout << "JSON saved to " << file.GetName() << '.' << std::endl;
        break;
      } else if (c == 'N' || c == 'n') {
        std::cout << "Cancelled." << std::endl;
        break;
      } else {
        std::cout << "Unknown command y, Y, n or N expected but found: "
                  << c << std::endl;
      }
    } while (true);
  } else {
    file.Save(json, pretty);
    std::cout << "JSON saved to " << file.GetName() << '.' << std::endl;
  }
}

void JsonParser::PrintToStdin(const JsonValue& json, bool pretty) {
  json.Serialize(std::cout, pretty);
  std::cout << std::endl;
}
