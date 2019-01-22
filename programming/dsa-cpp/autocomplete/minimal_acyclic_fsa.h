#ifndef AUTOCOMPLETE_FSA_H
#define AUTOCOMPLETE_FSA_H

#include <unordered_map>
#include <vector>
#include <cassert>

template<typename AlphabetType>
class MinimalAcyclicFSA {

  enum StateType {
    FINAL, EMPTY,
  };

  struct State {
    int id;

  };

  using TransitionMap = std::unordered_map<AlphabetType, State>;
  using StateMap = std::unordered_map<State, TransitionMap>;

  using StateIt = typename StateMap::iterator;
  using TransitionIt = typename TransitionMap::iterator;

  using Word = std::basic_string<AlphabetType>;

  StateMap state_map;
  StateType start_state;
  StateType empty_state;
 public:

  MinimalAcyclicFSA() : empty_state(0) {};
  MinimalAcyclicFSA(const MinimalAcyclicFSA&) = default;
  MinimalAcyclicFSA& operator=(const MinimalAcyclicFSA&) = default;
  ~MinimalAcyclicFSA() = default;

  StateType delta(StateType state, AlphabetType letter) const {
    StateIt state_it = state_map.find(state);

    if (state_it == state_map.end())
      return empty_state;

    const TransitionMap& transition_map = state_it->second;
    TransitionIt transition_it = transition_map.find(letter);

    assert(transition_it != transition_map.end() && "delta: letter should be in the transition map.");

    return transition_it->second;
  }

  StateType commonPrefix(const Word& word, Word& prefix) const {
    StateType current_state = start_state;

    for (size_t i = 0; i < word.length(); ++i) {
      StateType next_state = delta(current_state, word[i]);

      if (next_state == empty_state)
        return current_state;

      prefix.push_back(word[i]);
    }

    return current_state;
  }

  void addWord(const Word& word) {
    Word common_prefix;
    StateType last_state = commonPrefix(word, common_prefix);
    Word current_suffix = word.substr(common_prefix.length() + 1);

    if (current_suffix.empty() && )
  }
};

#endif //AUTOCOMPLETE_FSA_H
