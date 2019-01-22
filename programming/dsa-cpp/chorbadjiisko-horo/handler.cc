//
// Created by valio_stoyanov on 12/14/18.
//

#include <iostream>
#include "handler.h"

Handler::Handler(const Dance& dance) : dance_(dance) {}

void Handler::Release(const std::string& who, Hold hold) {
  if (dance_.SetHold(who, hold, false))
    std::cout << "RELEASE succeeded." << std::endl;
  else
    std::cout << "RELEASE failed." << std::endl;
}

void Handler::Grab(const std::string& who, Hold hold) {
  if (dance_.SetHold(who, hold, true))
    std::cout << "GRAB succeeded." << std::endl;
  else
    std::cout << "GRAB failed." << std::endl;
}

void Handler::Info(const std::string& who) const {
  std::cout << dance_.GetInfo(who) << std::endl;
}

void Handler::Add(const std::string& who,
                  const std::string& left,
                  const std::string& right) {
  if (dance_.Add(who, left, right))
    std::cout << "ADD succeeded." << std::endl;
  else
    std::cout << "ADD failed." << std::endl;
}

void Handler::Remove(const std::string& who) {
  if (dance_.Remove(who))
    std::cout << "Free at last!" << std::endl;
  else
    std::cout << "This won't be so easy!" << std::endl;
}

void Handler::Swap(const std::string& who1, const std::string& who2) {
  if (  dance_.Swap(who1, who2))
    std::cout << "SWAP succeeded." << std::endl;
  else
    std::cout << "SWAP failed." << std::endl;
}

void Handler::Print() const {
  std::cout << dance_ << std::endl;
}

void Handler::Exit() const {
  std::cout << ";)" << std::endl;
}

void Handler::InputErr(const std::string& msg) const {
  std::cerr << "Input error: " << msg << std::endl;
}
