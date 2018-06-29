//
// Created by valio_stoyanov on 6/29/18.
//

#include "json_token.h"

const char JsonToken::kBegObjCh = '{';
const char JsonToken::kEndObjCh = '}';
const char JsonToken::kBegArrCh = '[';
const char JsonToken::kEndArrCh = ']';
const char JsonToken::kValueSeparatorCh = ',';
const char JsonToken::kColonSeparatorCh = ':';
const char JsonToken::kStrCh = '\"';
const char* JsonToken::kNullStr = "null";
const char* JsonToken::kTrueStr = "true";
const char* JsonToken::kFalseStr = "false";