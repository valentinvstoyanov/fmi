#ifndef PLAYFAIR_CIPHER__RANDOM_CODER_H_
#define PLAYFAIR_CIPHER__RANDOM_CODER_H_

#include <array>
#include <cassert>
#include <algorithm>
#include <ctime>

class RandomCoder {
 public:
  static const int MATRIX_SIZE = 5;
  static const int ALPHABET_SIZE = 26;

  using Matrix = std::array<std::array<char, MATRIX_SIZE>, MATRIX_SIZE>;
  using Alphabet = std::array<char, ALPHABET_SIZE>;

  RandomCoder();

  std::string encode(std::string) const;
  std::string decode(const std::string&) const;
 private:
  void encodeBlockIn(char, char, std::string&) const;
  void decodeBlockIn(char, char, std::string&) const;
  std::pair<int, int> coordsOf(char) const;
  int shift(int, bool) const;

  Matrix matrix{};
  Alphabet alphabet{};

  char excluded_letter;
  char excluded_replacement_letter;

  char filling_letter;
  char exchange_letter;
};

RandomCoder::RandomCoder()
    : excluded_letter('J'),
      excluded_replacement_letter('I'),
      filling_letter('X'),
      exchange_letter('Z') {
  for (char ch = 'A'; ch <= 'Z'; ++ch) alphabet[ch - 'A'] = ch;

  std::srand(std::time(NULL));
  std::random_shuffle(alphabet.begin(), alphabet.end());

  int abCounter = 0;
  for (int i = 0; i < MATRIX_SIZE; ++i) {
    for (int j = 0; j < MATRIX_SIZE; ++j) {
      if (alphabet[abCounter] == excluded_letter) ++abCounter;
      matrix[i][j] = alphabet[abCounter];
      ++abCounter;
    }
  }

  std::sort(alphabet.begin(), alphabet.end());
}

std::string RandomCoder::encode(std::string text) const {
  std::replace(text.begin(), text.end(), excluded_letter, excluded_replacement_letter);
  std::string cryptotext;
  for(int i = 0; i < text.size();) {
    const char curr = text[i];
    const int si = i + 1;
    const bool curr_eq_next = curr == text[si];
    const char next = si < text.size() ? (curr_eq_next ? exchange_letter : text[si]) : filling_letter;
    encodeBlockIn(curr, next, cryptotext);
    i = curr_eq_next ? si : si + 1;
  }

  return cryptotext;
}

std::string RandomCoder::decode(const std::string& cryptotext) const {
  std::string text;
  for(int i = 0; i < cryptotext.size() - 1; i += 2) {
    const char curr = cryptotext[i];
    const int si = i + 1;
    const char next = cryptotext[si];
    decodeBlockIn(curr, next, text);
  }

  return text;
}

void RandomCoder::encodeBlockIn(char a, char b, std::string& buff) const {
  std::pair<int, int> a_coord = coordsOf(a);
  std::pair<int, int> b_coord = coordsOf(b);

  std::pair<int, int> corresponding_a_coord;
  std::pair<int, int> corresponding_b_coord;

  if (a_coord.first == b_coord.first) {
    corresponding_a_coord.first = corresponding_b_coord.first = a_coord.first;
    corresponding_a_coord.second = shift(a_coord.second, true);
    corresponding_b_coord.second = shift(b_coord.second, true);
  } else if (a_coord.second == b_coord.second) {
    corresponding_a_coord.second = corresponding_b_coord.second = a_coord.second;
    corresponding_a_coord.first = shift(a_coord.first, true);
    corresponding_b_coord.first = shift(b_coord.first, true);
  } else {
    corresponding_a_coord.first = a_coord.first;
    corresponding_a_coord.second = b_coord.second;
    corresponding_b_coord.first = b_coord.first;
    corresponding_b_coord.second = a_coord.second;
  }

  char corresponding_a_char = matrix[corresponding_a_coord.first][corresponding_a_coord.second];
  char corresponding_b_char = matrix[corresponding_b_coord.first][corresponding_b_coord.second];

  buff.push_back(corresponding_a_char);
  buff.push_back(corresponding_b_char);
}

void RandomCoder::decodeBlockIn(char a, char b, std::string& buff) const {
  std::pair<int, int> a_coord = coordsOf(a);
  std::pair<int, int> b_coord = coordsOf(b);

  std::pair<int, int> corresponding_a_coord;
  std::pair<int, int> corresponding_b_coord;

  if (a_coord.first == b_coord.first) {
    corresponding_a_coord.first = corresponding_b_coord.first = a_coord.first;
    corresponding_a_coord.second = shift(a_coord.second, false);
    corresponding_b_coord.second = shift(b_coord.second, false);
  } else if (a_coord.second == b_coord.second) {
    corresponding_a_coord.second = corresponding_b_coord.second = a_coord.second;
    corresponding_a_coord.first = shift(a_coord.first, false);
    corresponding_b_coord.first = shift(b_coord.first, false);
  } else {
    corresponding_a_coord.first = a_coord.first;
    corresponding_a_coord.second = b_coord.second;
    corresponding_b_coord.first = b_coord.first;
    corresponding_b_coord.second = a_coord.second;
  }

  char corresponding_a_char = matrix[corresponding_a_coord.first][corresponding_a_coord.second];
  char corresponding_b_char = matrix[corresponding_b_coord.first][corresponding_b_coord.second];

  buff.push_back(corresponding_a_char);
  buff.push_back(corresponding_b_char);
}

std::pair<int, int> RandomCoder::coordsOf(char c) const {
  for (int i = 0; i < MATRIX_SIZE; ++i)
    for (int j = 0; j < MATRIX_SIZE; ++j)
      if (matrix[i][j] == c) return {i, j};

  assert(false);
}

int RandomCoder::shift(int i, bool positive) const {
  return (positive ? i + 1 : i - 1) % MATRIX_SIZE;
}

#endif //PLAYFAIR_CIPHER__RANDOM_CODER_H_
