//
// Created by valentinvstoyanov on 17.04.20 г..
//

#ifndef PUZZLE__BOARD_BUILDER_H_
#define PUZZLE__BOARD_BUILDER_H_

#include <array>
#include <cstdlib>
#include <ctime>
#include <limits>
#include <algorithm>
#include "board.h"

template <size_t SIZE>
class BoardBuilder {
 public:
  BoardBuilder();

  static Board<SIZE> createRandomBoard(unsigned upper_limit = 0);
  static Board<SIZE> createRandomBoard(std::array<unsigned, (SIZE * SIZE) - 1>);

  bool canAddMore() const;
  BoardBuilder& addAt(unsigned, size_t, size_t);
  bool addIfPossibleAt(unsigned, size_t, size_t);

  Board<SIZE> build() const;
 private:
  using Matrix = typename Board<SIZE>::Matrix;

  void updateEmptyPos(size_t x, size_t y);

  size_t additions_count;
  Matrix matrix;
  std::pair<size_t, size_t> empty_pos;
};

template<size_t SIZE>
BoardBuilder<SIZE>::BoardBuilder()
    : additions_count(0),
      empty_pos(std::make_pair(0, 0)) {
  for(auto& row: matrix) row.fill(-1);
}

template<size_t SIZE>
Board<SIZE> BoardBuilder<SIZE>::createRandomBoard(unsigned upper_limit) {
  std::srand(std::time(nullptr));
  std::array<unsigned, (SIZE * SIZE) - 1> rndVals;
  upper_limit = upper_limit == 0 ? std::numeric_limits<unsigned>::max() : upper_limit;
  std::generate(rndVals.begin(), rndVals.end(), [upper_limit]() { return std::rand() % upper_limit; });
  return createRandomBoard(rndVals);
}

template<size_t SIZE>
Board<SIZE> BoardBuilder<SIZE>::createRandomBoard(std::array<unsigned, (SIZE * SIZE) - 1> within) {
  std::srand(std::time(nullptr));
  std::random_shuffle(within.begin(), within.end());
  const size_t empty_pos_x = std::rand() % SIZE;
  const size_t empty_pos_y = std::rand() % SIZE;
  int w_counter = 0;

  BoardBuilder<SIZE> builder;
  for (size_t i = 0; i < SIZE; ++i) {
    for (size_t j = 0; j < SIZE; ++j) {
      if (i == empty_pos_x && j == empty_pos_y) continue;
      builder.addAt(within[w_counter++], i, j);
    }
  }

  return builder.build();
}

template<size_t SIZE>
bool BoardBuilder<SIZE>::canAddMore() const {
  return additions_count < (SIZE * SIZE) - 1;
}

template<size_t SIZE>
BoardBuilder<SIZE>& BoardBuilder<SIZE>::addAt(unsigned n, size_t x, size_t y) {
  assert(canAddMore());
  matrix[x][y] = n;
  updateEmptyPos(x, y);
  ++additions_count;
  return *this;
}

template<size_t SIZE>
bool BoardBuilder<SIZE>::addIfPossibleAt(unsigned n, size_t x, size_t y) {
  if (canAddMore()) {
    matrix[x][y] = n;
    updateEmptyPos(x, y);
    ++additions_count;
    return true;
  }

  return false;
}

template<size_t SIZE>
Board<SIZE> BoardBuilder<SIZE>::build() const {
  return Board<SIZE>(matrix, empty_pos);
}

template<size_t SIZE>
void BoardBuilder<SIZE>::updateEmptyPos(size_t x, size_t y) {
  assert(canAddMore());

  if (empty_pos.first == x && empty_pos.second == y) {
    if (y == matrix.size() - 1) {
      ++empty_pos.first;
      empty_pos.second = 0;
      return;
    }

    ++empty_pos.second;
  }
}

#endif //PUZZLE__BOARD_BUILDER_H_
