//
// Created by valio_stoyanov on 12/12/18.
//

#ifndef CHORBADJIISKO_HORO_DANCE_H
#define CHORBADJIISKO_HORO_DANCE_H

#include <ostream>
#include <istream>
#include "hash_table.h"
#include "hold.h"
#include "dancer.h"
#include "hash_function.h"

class Dance {
 public:
  using Table = HashTable<std::string, Dancer, HashFunction, std::equal_to<>>;

 private:
  Table table_;
  std::string leader_label_;

  bool AreNeighbours(const Dancer& left, const Dancer& right) const {
    return left.right_label_ == right.label_ && right.left_label_ == left.label_;
  }

  bool IsFree(const Dancer& dancer) const {
    return !table_.Get(dancer.left_label_).holds_right_ &&
        !table_.Get(dancer.right_label_).holds_left_;
  }

  bool AreSwappable(const Dancer& left, const Dancer& right) const {
    return AreNeighbours(left, right) &&
        (!left.holds_left_ && !right.holds_right_) &&
        (IsFree(left) && IsFree(right));
  }

 public:
  Dance() : table_(Table(HashFunction(), std::equal_to<>())) {}

  bool SetHold(const std::string& who, Hold hold, bool b) {
    if (table_.Contains(who)) {
      Dancer& dancer = table_.Get(who);
      if (hold == kLeft) dancer.holds_left_ = b;
      else if (hold == kRight) dancer.holds_right_ = b;
      else dancer.SetHoldsBoth(b);
      return true;
    }

    return false;
  }

  std::string GetInfo(const std::string& who) const {
    if (table_.Contains(who)) {
      const Dancer& dancer = table_.Get(who);

      const Dancer& left_dancer = table_.Get(dancer.left_label_);
      const Dancer& right_dancer = table_.Get(dancer.right_label_);
      std::string acc;

      acc += dancer.left_label_;
      acc += dancer.holds_left_ ? " <--" : " ---";

      acc += left_dancer.holds_right_ ? "> " : "- ";
      acc += who;
      acc += right_dancer.holds_left_ ? " <--" : " ---";
      acc += dancer.holds_right_ ? "> " : "- ";
      acc += dancer.right_label_;

      return acc;
    }

    return "";
  }

  bool Add(const std::string& who,
           const std::string& left_label,
           const std::string& right_label) {
    if (table_.Contains(who)) return false;

    if (table_.Contains(left_label)) {
      if (table_.Contains(right_label)) {
        Dancer& left_dancer = table_.Get(left_label);
        Dancer& right_dancer = table_.Get(right_label);

        if (!AreNeighbours(left_dancer, right_dancer)) return false;

        Dancer dancer(who, left_dancer.label_, right_dancer.label_);
        table_ += std::make_pair(who, dancer);
        left_dancer.right_label_ = who;
        right_dancer.left_label_ = who;

        return true;
      }
    }

    return false;
  }

  bool Remove(const std::string& who) {
    if (table_.Contains(who)) {
      Dancer& dancer = table_.Get(who);

      if (dancer.HoldsSomeone() || !IsFree(dancer))
        return false;

      Dancer& left = table_.Get(dancer.left_label_);
      Dancer& right = table_.Get(dancer.right_label_);

      left.right_label_ = right.label_;
      right.left_label_ = left.label_;

      left.holds_right_ = right.holds_left_ = true;

      table_.Remove(who);

      if (who == leader_label_)
        leader_label_ = right.label_;

      return true;
    }

    return false;
  }

  bool Swap(const std::string& who1, const std::string& who2) {
    if (table_.Contains(who1) && table_.Contains(who2)) {
      Dancer& dancer1 = table_.Get(who1);
      Dancer& dancer2 = table_.Get(who2);

      if (!(AreSwappable(dancer1, dancer2) && AreSwappable(dancer2, dancer1)))
        return false;

      std::swap(dancer1, dancer2);

      if (who1 == leader_label_)
        leader_label_ = who2;

      return true;
    }

    return false;
  }

  friend std::ostream& operator<<(std::ostream& out, const Dance& dance) {
    Dancer current = dance.table_.Get(dance.leader_label_);
    do {
      out << current.label_;
      current = dance.table_.Get(current.right_label_);
      if (current.label_ == dance.leader_label_) break;
      out << '\n';
    } while (true);
    return out;
  }

  friend std::istream& operator>>(std::istream& in, Dance& dance) {
    std::string first;
    std::getline(in, first);
    dance.table_ += std::make_pair(first, Dancer(first, "", ""));

    std::string prev_line = first;
    std::string curr_line;
    while (std::getline(in, curr_line)) {
      dance.table_ += std::make_pair(curr_line, Dancer(curr_line, prev_line, first));
      dance.table_.Get(prev_line).right_label_ = curr_line;
      dance.table_.Get(first).left_label_ = curr_line;
      prev_line = curr_line;
    }

    dance.leader_label_ = first;

    return in;
  }
};

#endif //CHORBADJIISKO_HORO_DANCE_H
