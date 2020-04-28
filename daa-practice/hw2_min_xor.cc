#include<iostream>

using namespace std;

int minimalXor = 2000000000;

struct Node {
        int key;
        Node* left;
	Node* right;
};

Node* newNode(int item) {
        return new Node{item, NULL, NULL};
}

bool search(Node* node, int key) {
        if (!node) {
                return false;
        }

        if (key == node->key) {
		return true;
	}

	return key < node->key ? search(node->left, key) : search(node->right, key);
}


Node* insert(Node* node, int key) {
        if (!node) {
		return newNode(key);
	}

        int tempXor = key ^ node->key;
        if (tempXor < minimalXor) {
                minimalXor = tempXor;
        }
        
        if (key < node->key) {
                node->left = insert(node->left, key);
        } else if (key > node->key) {
                node->right = insert(node->right, key);
	}

        return node;
}

int main()
{        
        int N;
        std::cin >> N;
        Node *root = NULL;
        root = insert(root, 0);

        while(N--) {
                int num;
                cin >> num;

                bool found = search(root, num);
                if (!found) {
                        insert(root, num);
                }

                cout << minimalXor << endl;
        }

        return 0;
}
