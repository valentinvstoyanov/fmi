//
// Created by valentinvstoyanov on 18.04.20 г..
//

#ifndef PUZZLE__PUZZLE_SOLVER_H_
#define PUZZLE__PUZZLE_SOLVER_H_

#include <vector>
#include <queue>
#include <functional>
#include <unordered_map>
#include <unordered_set>
#include "board.h"

template<size_t SIZE>
class Puzzle8Solver {
 public:
  using Cost = size_t;
  using Heuristic = std::function<Cost(const Board<SIZE>&, const Board<SIZE>&)>;
  explicit Puzzle8Solver(Heuristic);

  void solve(const Board<SIZE>&, const Board<SIZE>&);
  bool hasSolution() const;
  const std::vector<Board<SIZE>>& solution() const;
 private:
  using BoardWithCost = std::pair<Board<SIZE>, Cost>;
  using BoardBoardMap = std::unordered_map<Board<SIZE>, Board<SIZE>>;
  using BoardCostMap = std::unordered_map<Board<SIZE>, Cost>;

  struct CompareBoardsByCost {
    bool operator()(const BoardWithCost& p1, const BoardWithCost& p2) const;
  };

  void foundSolution(const Board<SIZE>&, const BoardBoardMap&);
  void buildPath(const Board<SIZE>&, const BoardBoardMap&);

  std::vector<Board<SIZE>> solutionPath;
  Heuristic heuristic;
};

template<size_t SIZE>
Puzzle8Solver<SIZE>::Puzzle8Solver(Heuristic h) : heuristic(h), solutionPath(std::vector<Board<SIZE>>(0)) {}

template<size_t SIZE>
void Puzzle8Solver<SIZE>::solve(const Board<SIZE>& src, const Board<SIZE>& dest) {
  std::priority_queue<BoardWithCost, std::vector<BoardWithCost>, CompareBoardsByCost> front;
  BoardBoardMap parent;
  BoardCostMap cost;

  front.push({src, 0});
  parent.insert({src, src});
  cost.insert({src, 0});

  while (!front.empty()) {
    const auto current = front.top().first;
    front.pop();

    if (current == dest) {
      foundSolution(current, parent);
      return;
    }

    const auto& neighbours = current.moveToAllDirections();
    for (const auto& next: neighbours) {
      const Cost new_cost = cost[current] + 1;

      if (cost.find(next) == cost.end() || new_cost < cost[next]) {
        cost[next] = new_cost;
        parent[next] = current;
        front.push({next, new_cost + heuristic(next, dest)});
      }
    }
  }
}

template<size_t SIZE>
bool Puzzle8Solver<SIZE>::hasSolution() const {
  return !solutionPath.empty();
}

template<size_t SIZE>
const std::vector<Board<SIZE>>& Puzzle8Solver<SIZE>::solution() const {
  assert(hasSolution());
  return solutionPath;
}

template<size_t SIZE>
void Puzzle8Solver<SIZE>::foundSolution(const Board<SIZE>& dest, const BoardBoardMap& parent) {
  solutionPath.clear();
  buildPath(dest, parent);
}

template<size_t SIZE>
void Puzzle8Solver<SIZE>::buildPath(const Board<SIZE>& dest, const BoardBoardMap& parent) {
  auto pIt = parent.find(dest);
  assert(pIt != parent.end());

  if (pIt->second == dest) {
    solutionPath.push_back(dest);
    return;
  }

  buildPath(pIt->second, parent);
  solutionPath.push_back(dest);
}

template<size_t SIZE>
bool Puzzle8Solver<SIZE>::CompareBoardsByCost::operator()(const BoardWithCost& p1, const BoardWithCost& p2) const {
  return p1.second > p2.second;
}

#endif //PUZZLE__PUZZLE_SOLVER_H_
