#include <iostream>
#include <fstream>
#include <ctime>
#include <cassert>
#include <string>

struct Student {
  unsigned id;
  std::string name;
};

class Queue {
 public:
  struct Customer {
    Student student;
    time_t time;
  };

 private:
  struct Node {
    Customer customer;
    Node* next;

    explicit Node(const Customer& c, Node* n = nullptr)
    : customer(c), next(n) {}
  };

  Node* head;

  void clear() {
    while (head)
      dequeue();
  }

  void copyQueue(const Queue& other) {
    if (!other.head)
      return;

    head = new Node(other.head->customer);

    Node* this_curr = head;
    Node* other_curr = other.head->next;
    while (other_curr) {
      this_curr->next = new Node(other_curr->customer);
      this_curr = this_curr->next;
      other_curr = other_curr->next;
    }
  }

  void incrementTimes(Node* node, unsigned time) {
    Node* curr = node;
    while (curr) {
      curr->customer.time += time;
      curr = curr->next;
    }
  }

 public:
  Queue() : head(nullptr) {}

  ~Queue() {
    clear();
  }

  Queue(const Queue& other)
      : head(nullptr) {
    copyQueue(other);
  }

  Queue& operator=(const Queue& other) {
    if (this != &other) {
      clear();
      copyQueue(other);
    }
    return *this;
  }

  Customer& front() {
    assert(head && "Front called on empty queue");
    return head->customer;
  }

  const Customer& front() const {
    assert(head && "Front called on empty queue");
    return head->customer;
  }

  bool empty() const {
    return !head;
  }

  bool enqueue(const Customer& customer) {
    if (empty()) {
      head = new Node(customer);
      return false;
    }

    Node* prev;
    Node* curr = head;
    while (curr) {
      if (curr->customer.student.id == customer.student.id) {
        Node* node = new Node(customer, curr->next);
        curr->next = node;

        //curr->customer.time += 60;
        //incrementTimes(curr->next->next, 60);

        return true;
      }

      prev = curr;
      curr = curr->next;
    }

    Node* node = new Node(customer, prev->next);
    prev->next = node;

    return false;
  }

  void incrementTimes(unsigned time) {
    incrementTimes(head, time);
  }

  void dequeue() {
    assert(head && "Dequeue called on empty queue");
    Node* front = head;
    head = head->next;
    delete front;
  }
};

int main() {
  const char* const kfilename = "students.txt";

  std::ifstream file(kfilename);
  if (file.is_open()) {
    Student student;
    Queue queue;
    const time_t start_time = time(NULL);
    int minute_counter = 0;


    while (file >> student.name && file >> student.id) {
      bool res = queue.enqueue({student, start_time});

      ++minute_counter;
      queue.incrementTimes(60);

      if (minute_counter == 2) {
        std::cout << queue.front().student.name << ' '
                  << ((int) (queue.front().time - start_time) / 60)
                  << 'm' << std::endl;
        queue.dequeue();
        minute_counter = 0;
      }
    }

    while (!queue.empty()) {
      ++minute_counter;
      queue.incrementTimes(60);

      if (minute_counter == 2) {
        std::cout << queue.front().student.name << ' '
                  << ((int) (queue.front().time - start_time) / 60)
                  << 'm' << std::endl;
        queue.dequeue();
        minute_counter = 0;
      }
    }

  } else {
    std::cerr << "Failed to open file: " << kfilename << std::endl;
  }

  return 0;
}