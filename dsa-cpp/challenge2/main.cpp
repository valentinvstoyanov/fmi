#include <iostream>
#include <vector>
#include <cassert>

struct Node {
  int value;
  Node* left;
  Node* right;

  explicit Node(int v, Node* l = nullptr, Node* r = nullptr)
  : value(v), left(l), right(r) {}
};

void walkInOrderBST(const Node* node, std::vector<int>& res) {
  if (!node)
    return;

  walkInOrderBST(node->left, res);
  res.push_back(node->value);
  walkInOrderBST(node->right, res);
}

void deleteBST(Node* node) {
  if (!node)
    return;

  deleteBST(node->left);
  deleteBST(node->right);
  delete node;
}

Node* arrToBST(const int* arr, size_t size) {
  if (!arr || size == 0)
    return nullptr;

  size_t middle_idx = size / 2;
  Node* node = new Node(arr[middle_idx]);
  node->left = arrToBST(arr, middle_idx);
  node->right = arrToBST(arr + middle_idx + 1, size - middle_idx - 1);

  return node;
}

void printVector(const std::vector<int>& vector) {
  for (int i = 0; i < vector.size(); ++i)
    std::cout << vector[i] << " ";
  std::cout << std::endl;
}

int main() {
  std::vector<int> arr = {7, 11, 23, 48, 53, 119, 2019};

  Node* tree = arrToBST(arr.data(), arr.size());

  std::vector<int> walk;
  walkInOrderBST(tree, walk);

  printVector(arr);
  printVector(walk);

  deleteBST(tree);

  assert(arr == walk && "arr != walk");

  return 0;
}