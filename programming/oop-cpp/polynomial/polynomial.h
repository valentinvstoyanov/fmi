//
// Created by valio_stoyanov on 5/31/18.
//

#ifndef POLYNOMIAL_POLYNOMIAL_H
#define POLYNOMIAL_POLYNOMIAL_H

#include <limits>
#include "ds/array.h"

/*
  TODO: Extract some of the operators code in properly named functions.
  TODO: Implement polynom simplification and call it.
  TODO: Think of algorithm for polynomial division.
  TODO: Add the rest features and check functionality.
*/

  template <typename T>
class Polynomial {
  Array<T> coefficients_;
  T PowerCoefficient(const T&, const size_t) const;
  T MonomialIndefiniteIntegral(const T&, const size_t, const T&, const T&) const;
 public:
  explicit Polynomial(const Array<T>&);
  Polynomial(const Polynomial&);

  Polynomial& operator=(const Polynomial&);
  bool operator==(const Polynomial&);
  bool operator!=(const Polynomial&);
  bool operator<(const Polynomial&);
  bool operator>(const Polynomial&);
  bool operator<=(const Polynomial&);
  bool operator>=(const Polynomial&);

  Polynomial& operator-=(const Polynomial&);
  Polynomial& operator+=(const Polynomial&);
  Polynomial& operator*=(const Polynomial&);
  Polynomial& operator/=(const Polynomial&);
  Polynomial& operator%=(const Polynomial&);

  Polynomial& operator*=(const T&);
  Polynomial& operator/=(const T&);

  const T& operator[](const size_t) const;
  T& operator[](const size_t);

  T operator()(const T&);
  T operator()(const T&, const T&);

  Polynomial<T>& operator++();
  Polynomial<T> operator++(int);
  Polynomial<T>& operator--();
  Polynomial<T> operator--(int);

  explicit operator int();
  explicit operator bool();
};

template<typename T>
Polynomial<T>::Polynomial(const Array<T>& coefficients)
    : coefficients_(coefficients) {}

template<typename T>
Polynomial<T>::Polynomial(const Polynomial& other)
    : coefficients_(other.coefficients_) {}

template<typename T>
Polynomial<T>& Polynomial<T>::operator=(const Polynomial& other) {
  if (this != &other)
    coefficients_ = other.coefficients_;

  return *this;
}

template<typename T>
bool Polynomial<T>::operator==(const Polynomial& other) {
  return coefficients_ == other.coefficients_;
}

template<typename T>
bool Polynomial<T>::operator!=(const Polynomial& other) {
  return !(operator==(other));
}

template<typename T>
bool Polynomial<T>::operator<(const Polynomial& other) {
  return coefficients_.Size() < other.coefficients_.Size();
}

template<typename T>
bool Polynomial<T>::operator>(const Polynomial& other) {
  return coefficients_.Size() > other.coefficients_.Size();
}

template<typename T>
bool Polynomial<T>::operator<=(const Polynomial& other) {
  return !(*this > other);
}

template<typename T>
bool Polynomial<T>::operator>=(const Polynomial& other) {
  return !(*this < other);
}

template<typename T>
Polynomial<T>::operator int() {
  return coefficients_.Empty() ? std::numeric_limits<int>::max()
                               : static_cast<int>(coefficients_.Size() - 1);
}

template<typename T>
Polynomial<T>& Polynomial<T>::operator-=(const Polynomial<T>& other) {
  const size_t min_size = std::min(coefficients_.Size(), other.coefficients_.Size());
  for (size_t i = 0; i < min_size; ++i)
    coefficients_[i] -= other.coefficients_[i];

  if (coefficients_.Size() < other.coefficients_.Size())
    for (size_t i = min_size; i < other.coefficients_.Size(); ++i)
      coefficients_.PushBack(-other.coefficients_[i]);

  //TODO: simplify

  return *this;
}

template<typename T>
Polynomial<T>& Polynomial<T>::operator+=(const Polynomial<T>& other) {
  const size_t min_size = std::min(coefficients_.Size(), other.coefficients_.Size());
  for (size_t i = 0; i < min_size; ++i)
    coefficients_[i] += other.coefficients_[i];

  if (coefficients_.Size() < other.coefficients_.Size())
    for (size_t i = min_size; i < other.coefficients_.Size(); ++i)
      coefficients_.PushBack(other.coefficients_[i]);

  //TODO: simplify

  return *this;
}

template<typename T>
Polynomial<T>& Polynomial<T>::operator*=(const Polynomial<T>& other) {
  const size_t result_deg = coefficients_.Size() + other.coefficients_.Size() - 1;
  Array<T> result(result_deg);
  //
  result.Fill(T(0));

  for (size_t i = 0; i < coefficients_.Size(); ++i)
    for (size_t j = 0; j < other.coefficients_.Size(); ++j)
      result[i + j] += coefficients_[i] * other.coefficients_[j];

  coefficients_ = result;
  //TODO: simplify

  return *this;
}

template<typename T>
Polynomial<T>& Polynomial<T>::operator/=(const Polynomial<T>& other) {
  //TODO
  return *this;
}

template<typename T>
Polynomial<T>& Polynomial<T>::operator%=(const Polynomial<T>&) {
  //TODO
  return *this;
}

template<typename T>
Polynomial& Polynomial<T>::operator*=(const T& val) {
  for (Array<T>::Iterator iterator = coefficients_.Begin();
      iterator != coefficients_.End();
      ++iterator)
    *iterator *= val;

  return *this;
}

template<typename T>
Polynomial& Polynomial<T>::operator/=(const T& val) {
  for (Array<T>::Iterator iterator = coefficients_.Begin();
       iterator != coefficients_.End();
       ++iterator)
    *iterator /= val;

  return *this;
}

template<typename T>
const T& Polynomial<T>::operator[](const size_t index) const {
  if (coefficients_.Empty()) return 0;
  if (index >= coefficients_.Size()) return 0;
  return coefficients_[index];
}

template<typename T>
T& Polynomial<T>::operator[](const size_t index) {
  if (coefficients_.Empty()) return 0;
  if (index >= coefficients_.Size()) return 0;
  return coefficients_[index];
}

template<typename T>
T Polynomial<T>::PowerCoefficient(const T& coefficient, const size_t power) const {
  if (power == 0) return T(1);
  if (power == 1) return coefficient;

  T result = coefficient;
  for (size_t i = 0; i < power; ++i)
    result *= coefficient;

  return result;
}

template<typename T>
T Polynomial<T>::operator()(const T& x) {
  const T kZero = T(0);
  if (coefficients_.Empty()) return kZero;

  T result = coefficients_.Front();
  for (size_t i = 1; i < coefficients_.Size(); ++i)
    if (coefficients_[i] != kZero)
      result += coefficients_[i] * PowerCoefficient(x, i);

  return result;
}

template<typename T>
T Polynomial<T>::MonomialIndefiniteIntegral(const T& coefficient,
                                            const size_t power,
                                            const T& a,
                                            const T& b) const {
  if (coefficient == T(0)) return coefficient;
  const size_t new_power = power + 1;
  T new_coefficient = coefficient / new_power;
  T b_value = PowerCoefficient(b, new_power);
  T a_value = PowerCoefficient(a, new_power);

  return new_coefficient * (b_value - a_value);
}

template<typename T>
T Polynomial<T>::operator()(const T& a, const T& b) {
  const T kZero = T(0);
  if (coefficients_.Empty()) return kZero;

  T result(0);
  for (size_t i = 0; i < coefficients_.Size(); ++i)
    result += MonomialIndefiniteIntegral(coefficients_[i], i, a, b);

  return result;
}

template<typename T>
Polynomial<T>& Polynomial<T>::operator++() {
  Array<T> result(coefficients_.Size() + 1);
  result.PushBack(T(0));
  for (size_t i = 0; i < coefficients_.Size(); ++i)
    result.PushBack(coefficients_[i] / (i + 1));

  coefficients_ = result;
  //TODO: simplify

  return *this;
}

template<typename T>
Polynomial<T> Polynomial<T>::operator++(int) {
  Polynomial<T> result(*this);
  ++(*this);
  return result;
}

template<typename T>
Polynomial<T>& Polynomial<T>::operator--() {
  if (coefficients_.Empty()) return *this;
  if (coefficients_.Size() == 1) {
    coefficients_.PopBack();
    return *this;
  }

  Array<T> result(coefficients_.Size() - 1);
  result.PushBack(T(0));
  for (size_t i = 1; i < coefficients_.Size(); ++i)
    result.PushBack(coefficients_[i] * (i - 1));

  coefficients_ = result;
  //TODO: simplify

  return *this;
}

template<typename T>
Polynomial<T> Polynomial<T>::operator--(int) {
  Polynomial<T> result(*this);
  --(*this);

  return result;
}

template<typename T>
Polynomial<T>::operator bool() {
  //TODO:

  return false;
}

template<typename T>
Polynomial<T> operator+(const Polynomial<T>& lhs, const Polynomial<T>& rhs) {
  Polynomial<T> result = lhs;
  result += rhs;
  return result;
}

template<typename T>
Polynomial<T> operator-(const Polynomial<T>& lhs, const Polynomial<T>& rhs) {
  Polynomial<T> result = lhs;
  result -= rhs;
  return result;
}

template<typename T>
Polynomial<T> operator*(const Polynomial<T>& lhs, const Polynomial<T>& rhs) {
  Polynomial<T> result = lhs;
  result *= rhs;
  return result;
}

template<typename T>
Polynomial<T> operator/(const Polynomial<T>& lhs, const Polynomial<T>& rhs) {
  Polynomial<T> result = lhs;
  result /= rhs;
  return result;
}

template<typename T>
Polynomial<T> operator%(const Polynomial<T>& lhs, const Polynomial<T>& rhs) {
  Polynomial<T> result = lhs;
  result %= rhs;
  return result;
}

#endif //POLYNOMIAL_POLYNOMIAL_H
