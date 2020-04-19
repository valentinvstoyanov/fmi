//
// Created by valentinvstoyanov on 17.04.20 г..
//

#ifndef PUZZLE_BOARD_H
#define PUZZLE_BOARD_H

#include <array>
#include <vector>
#include <cassert>
#include <iostream>

/*All the values in the board are non-negative except for the empty one.*/
template<size_t SIZE>
class Board {
 public:
  enum Direction { kUp, kDown, kLeft, kRight };
  using Matrix = std::array<std::array<int, SIZE>, SIZE>;

  Board() = default;
  Board(const Matrix&, const std::pair<size_t, size_t>&);

  bool canMoveTo(Direction) const;
  Board moveTo(Direction) const;
  std::vector<Board> moveToAllDirections() const;

  std::pair<size_t, size_t> emptyPos() const;
  size_t emptyX() const;
  size_t emptyY() const;

  size_t size() const;

  int operator()(size_t, size_t) const;
  bool operator==(const Board<SIZE>&) const;
  bool operator!=(const Board<SIZE>&) const;

  template <size_t S>
  friend std::ostream& operator<<(std::ostream&, const Board<S>&);
 private:
  std::pair<size_t, size_t> calculateNextEmptyPos(Direction) const;

  std::array<std::array<int, SIZE>, SIZE> matrix;
  std::pair<size_t, size_t> empty_pos;
};

template<size_t SIZE>
Board<SIZE>::Board(const std::array<std::array<int, SIZE>, SIZE>& m, const std::pair<size_t, size_t>& p)
        : matrix(m), empty_pos(p) {
    assert(p.first < m.size() && p.second < m.front().size());
    assert(m[p.first][p.second] < 0);

    for (int i = 0; i < m.size(); ++i) {
        for (int j = 0; j < m[i].size(); ++j) {
            if (i == p.first && j == p.second) continue;
            assert(m[i][j] >= 0);
        }
    }
}

template<size_t SIZE>
bool Board<SIZE>::canMoveTo(Board::Direction d) const {
    switch (d) {
        case kUp: return emptyX() > 0;
        case kDown: return emptyX() < matrix.size() - 1;
        case kLeft: return emptyY() > 0;
        case kRight: return emptyY() < matrix.size() - 1;
    }

    assert(false);
}

template<size_t SIZE>
Board<SIZE> Board<SIZE>::moveTo(Board::Direction d) const {
    assert(canMoveTo(d));

    auto moved = matrix;
    auto moved_empty_pos = calculateNextEmptyPos(d);
    std::swap(moved[emptyX()][emptyY()], moved[moved_empty_pos.first][moved_empty_pos.second]);

    return Board(moved, moved_empty_pos);
}

template<size_t SIZE>
std::vector<Board<SIZE>> Board<SIZE>::moveToAllDirections() const {
  std::vector<Board<SIZE>> res;
  if(canMoveTo(kLeft)) res.push_back(moveTo(kLeft));
  if(canMoveTo(kRight)) res.push_back(moveTo(kRight));
  if(canMoveTo(kUp)) res.push_back(moveTo(kUp));
  if(canMoveTo(kDown)) res.push_back(moveTo(kDown));
  return res;
}

template<size_t SIZE>
std::pair<size_t, size_t> Board<SIZE>::emptyPos() const {
    return empty_pos;
}

template<size_t SIZE>
size_t Board<SIZE>::emptyX() const {
    return empty_pos.first;
}

template<size_t SIZE>
size_t Board<SIZE>::emptyY() const {
    return empty_pos.second;
}

template<size_t SIZE>
size_t Board<SIZE>::size() const {
  return matrix.size();
}

template<size_t SIZE>
int Board<SIZE>::operator()(size_t x, size_t y) const {
    return matrix[x][y];
}

template<size_t SIZE>
std::ostream& operator<<(std::ostream& os, const Board<SIZE>& b) {
  for (size_t i = 0; i < b.size(); ++i) {
    for (size_t j = 0; j < b.size(); ++j)
      os << b(i, j) << ' ';

    if (i != b.size() - 1) os << '\n';
  }

  return os;
}

template<size_t SIZE>
std::pair<size_t, size_t> Board<SIZE>::calculateNextEmptyPos(Board::Direction d) const {
    assert(canMoveTo(d));

    auto pos = empty_pos;
    switch (d) {
        case kUp: --pos.first;
            break;
        case kDown: ++pos.first;
            break;
        case kLeft: --pos.second;
            break;
        case kRight: ++pos.second;
            break;
    }

    assert(emptyX() != pos.first || emptyY() != pos.second);
    return pos;
}

template<size_t SIZE>
bool Board<SIZE>::operator==(const Board<SIZE>& o) const {
  if (emptyX() != o.emptyX() || emptyY() != o.emptyY()) return false;

  for(size_t i = 0; i < size(); ++i)
    for(size_t j = 0; j < size(); ++j)
      if (matrix[i][j] != o.matrix[i][j]) return false;

  return true;
}

template<size_t SIZE>
bool Board<SIZE>::operator!=(const Board<SIZE>& o) const {
  return !(*this == o);
}

namespace std {
template <size_t SIZE>
struct hash<Board<SIZE>>
{
  size_t operator()(const Board<SIZE>& b) const
  {
    size_t res = 0;
    for(size_t i = 0; i < SIZE; ++i)
      for(size_t j = 0; j < SIZE; ++j)
        res = res * 31 + std::hash<size_t>{}(b(i, j));
    res = res * 31 + (b.emptyX() * 10 + b.emptyY());
    return res;
  }
};
}

#endif //PUZZLE_BOARD_H
