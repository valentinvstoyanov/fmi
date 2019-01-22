#ifndef AUTOCOMPLETE_FSA_H
#define AUTOCOMPLETE_FSA_H

class FSA {

  using State = int;

  static const State EMPTY_STATE = 0;

  bool isEmpty(State state) {
    return state == EMPTY_STATE;
  }

  bool isFinal(State state) {
    return state < EMPTY_STATE;
  }

  State start;


 public:

  FSA() = default;
  ~FSA() = default;
  FSA(const FSA&) = default;
  FSA& operator=(const FSA&) = default;


};

#endif //AUTOCOMPLETE_FSA_H
