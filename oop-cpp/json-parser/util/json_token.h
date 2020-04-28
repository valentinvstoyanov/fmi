#ifndef JSON_PARSER_JSON_TOKEN_H
#define JSON_PARSER_JSON_TOKEN_H

class JsonToken {
 public:
  static const char kBegObjCh;
  static const char kEndObjCh;
  static const char kBegArrCh;
  static const char kEndArrCh;
  static const char kValueSeparatorCh;
  static const char kColonSeparatorCh;
  static const char kStrCh;
  static const char* kNullStr;
  static const char* kTrueStr;
  static const char* kFalseStr;
};

#endif //JSON_PARSER_JSON_TOKEN_H
