//
// Created by valio_stoyanov on 12/12/18.
//

#ifndef CHORBADJIISKO_HORO_HASHTABLE_H
#define CHORBADJIISKO_HORO_HASHTABLE_H

#include <vector>
#include <list>
#include <stdexcept>
#include <cassert>

template<typename K, typename V, typename Hash = std::hash<K>, typename KeyEqual = std::equal_to<K>>
class HashTable {
  using Pair = std::pair<K, V>;
  using Bucket = std::list<Pair>;
  using Table = std::vector<Bucket>;

  static const unsigned kInitialHeight = 128;
  static const unsigned kMaxWidth = 8;

  Table table_;
  Hash hasher_;
  KeyEqual comparator_;
  V default_;

  size_t CalculateHash(const K& key) const {
    return hasher_(key) % table_.size();
  }

  bool Compare(const K& k1, const K& k2) const {
    return comparator_(k1, k2);
  }

  void Rehash() {
    Table table(table_.size() * 2);
    table_.swap(table);
    for(const Bucket& bucket: table)
      for (const Pair& pair: bucket)
        table_[CalculateHash(pair.first)].push_front(pair);
  }

 public:
  explicit HashTable(const Hash& hasher = std::hash<K>(),
            const KeyEqual& comparator = std::equal_to<K>())
      : table_(kInitialHeight), hasher_(hasher), comparator_(comparator) {}

  bool Contains(const K& key) const {
    const Bucket& bucket = table_[CalculateHash(key)];
    typename Bucket::const_iterator it = bucket.cbegin();
    for (; it != bucket.cend(); ++it)
      if (Compare(it->first, key))
        return true;
    return false;
  }

  void Insert(const Pair& pair) {
    assert(!Contains(pair.first) && "Attempt to insert pair with already existing key.");

    size_t index = CalculateHash(pair.first);
    if (table_[index].size() >= kMaxWidth) {
      Rehash();
      index = CalculateHash(pair.first);
    }

    table_[index].push_front(pair);
  }

  void Insert(const K& key, const V& value) {
    Insert(Pair(key, value));
  }

  HashTable& operator+=(const Pair& pair) {
    Insert(pair);
    return *this;
  }

  V& Get(const K& key) {
    Bucket& bucket = table_[CalculateHash(key)];
    typename Bucket::iterator it = bucket.begin();
    for (; it != bucket.end(); ++it)
      if (Compare(it->first, key))
        return (it->second);
    throw std::runtime_error("Get value that is not in the hash table.");
  }

  const V& Get(const K& key) const {
    const Bucket& bucket = table_[CalculateHash(key)];
    typename Bucket::const_iterator it = bucket.cbegin();
    for (; it != bucket.cend(); ++it)
      if (Compare(it->first, key))
        return (it->second);
    throw std::runtime_error("Get value that is not in the hash table.");
  }

  void Remove(const K& key) {
    Bucket& bucket = table_[CalculateHash(key)];
    typename Bucket::iterator it = bucket.begin();
    for (; it != bucket.end(); ++it)
      if (Compare(it->first, key))
        bucket.erase(it);
  }
};

#endif //CHORBADJIISKO_HORO_HASHTABLE_H
