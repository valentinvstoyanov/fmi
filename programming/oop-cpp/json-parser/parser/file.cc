//
// Created by valio_stoyanov on 6/30/18.
//

#include <fstream>
#include "file.h"

File::File(const char* name) : name_(name) {}

File::File(const String& name) : name_(name) {}

String File::GetContent() const {
  static const unsigned chunk_size = 100;
  char chunk[chunk_size];
  std::ifstream f(name_.CStr());
  String content(chunk_size);
  while(f.getline(chunk, chunk_size))
    content.Append(String(chunk));
  if (!f.good())
    if (!f.eof())
      throw FileException("FileException: failed to get content from file.");

  return content;
}
