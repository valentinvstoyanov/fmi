#ifndef GAME_MAP_GRAPH_H
#define GAME_MAP_GRAPH_H

#include <unordered_map>
#include <unordered_set>
#include <functional>
#include <queue>
#include <tuple>

template<typename V, typename E>
class Graph {
  using EdgeMap = std::unordered_map<V, E>;
  using VertexMap = std::unordered_map<V, EdgeMap>;

  using EdgeCIterator = typename EdgeMap::const_iterator;
  using VertexIterator = typename VertexMap::iterator;

  VertexMap vertices;

  void buildNotVisitedVertexMap(const std::unordered_map<V, bool>& visited, VertexMap& result) const {

    for (auto it = visited.begin(); it != visited.end(); ++it)
      if (it->second == false) {
        //TODO: no const version of operator[]
        //result[it->first] = vertices[it->first];
      }
  }

 public:
  bool addVertex(const V& vertex) {
    VertexIterator v_it = vertices.find(vertex);
    if (v_it == vertices.end()) {
      vertices.insert(std::make_pair(vertex, EdgeMap()));
      return true;
    }

    return false;
  }

  bool addEdge(const V& from, const V& to, const E& edge) {
    VertexIterator from_it = vertices.find(from);
    if (from_it == vertices.end() || vertices.find(to) == vertices.end())
      return false;

    (from_it->second)[to] = edge;
    return true;
  }

  bool BFS(const V& start, std::function<bool(const V&, const E&, const V&)> on_visit_callback, bool need_not_visited, VertexMap& not_visited) const {
    if (vertices.find(start) == vertices.end()) {
      if (need_not_visited)
        not_visited = VertexMap();

      return false;
    }

    struct VisitData {
      const V* from;
      const E* edge;
      const V* to;
    };

    std::unordered_map<V, bool> visited;
    for (auto it = vertices.begin(); it != vertices.end(); ++it)
      visited[it->first] = false;

    //std::unordered_set<V> visited;

    std::queue<VisitData> queue;
    visited[start] = true;//.insert(start);
    queue.push(VisitData{nullptr, nullptr, &start});

    while (!queue.empty()) {
      VisitData triplet = queue.front();
      queue.pop();

      const V* from = triplet.from;
      const E* edge = triplet.edge;
      const V* to = triplet.to;

      if (from && !on_visit_callback(*from, *edge, *to)) {
        if (need_not_visited)
          buildNotVisitedVertexMap(visited, not_visited);
        return true;
      }

      visited[*to] = true;

      const EdgeMap& edge_map = vertices.find(*to)->second;
      for (EdgeCIterator edge_it = edge_map.begin(); edge_it != edge_map.end(); ++edge_it) {
        if (!visited[edge_it->first]) {
          visited[edge_it->first] = true;
          queue.push(VisitData{to, &(edge_it->second), &(edge_it->first)});
        }
      }
    }

    if (need_not_visited)
      buildNotVisitedVertexMap(visited, not_visited);

    return true;
  }
};

#endif //GAME_MAP_GRAPH_H
