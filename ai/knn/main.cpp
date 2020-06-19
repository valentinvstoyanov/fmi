#include <iostream>
#include <vector>
#include <functional>
#include <algorithm>

struct Company {
  int revenue;
  std::string teamCharacteristic;
  bool success;

  int getNumericTeamCharacteristic() const {
    if (teamCharacteristic.front() == 'V') return 4;
    else if (teamCharacteristic.front() == 'S') return 3;
    else if (teamCharacteristic.front() == 'A') return 2;
    else if (teamCharacteristic.front() == 'W') return 1;
    else throw std::runtime_error("Unknown team characteristic");
  }
};

using DistanceType = double;

struct Distance {
  DistanceType val;
  size_t companyIdx;

  bool operator<(const Distance& other) {
    return this->val < other.val;
  }
};

using CompanyDataset = std::vector<Company>;
using Distances = std::vector<Distance>;
using Metric = std::function<DistanceType(const Company&, const Company&)>;

Distances calculateDistances(const CompanyDataset& dataset, const Company& company, const Metric& metric) {
  Distances distances(dataset.size());
  for (size_t i = 0; i < dataset.size(); ++i) {
    distances[i].val = metric(company, dataset[i]);
    distances[i].companyIdx = i;
  }

  return distances;
}

bool classifyCompanySuccess(size_t k, const CompanyDataset& dataset, const Company& company, const Metric& metric) {
  Distances distances = calculateDistances(dataset, company, metric);
  std::sort(distances.begin(), distances.end(), std::less<>());

  size_t successFrequency = 0;
  size_t failFrequency = 0;

  for (size_t i = 0; i < k; ++i) {
    if (dataset[distances[i].companyIdx].success) {
      ++successFrequency;
    } else {
      ++failFrequency;
    }
  }

  return successFrequency > failFrequency;
}

CompanyDataset getCompanyDataset() {
  return {{982, "VeryStrong", 0},
          {1304, "VeryStrong", 1},
          {1256, "VeryStrong", 1},
          {1562, "VeryStrong", 1},
          {703, "VeryStrong", 0},
          {1213, "VeryStrong", 0},
          {1471, "VeryStrong", 1},
          {1315, "VeryStrong", 1},
          {691, "VeryStrong", 0},
          {1439, "VeryStrong", 1},
          {1377, "Strong", 1},
          {675, "Strong", 0},
          {1458, "Strong", 1},
          {1294, "Strong", 1},
          {611, "Strong", 0},
          {959, "Strong", 0},
          {1301, "Strong", 1},
          {1076, "Strong", 1},
          {1569, "Strong", 1},
          {680, "Strong", 0},
          {978, "Average", 1},
          {600, "Average", 0},
          {654, "Average", 0},
          {708, "Average", 0},
          {604, "Average", 0},
          {925, "Average", 0},
          {1213, "Average", 0},
          {1471, "Average", 1},
          {1215, "Average", 1},
          {1071, "Average", 0},
          {1569, "Weak", 1},
          {680, "Weak", 0},
          {952, "Weak", 1},
          {602, "Weak", 0},
          {654, "Weak", 0},
          {725, "Weak", 0},
          {604, "Weak", 0},
          {1256, "Weak", 1},
          {1404, "Weak", 1},
          {1256, "Weak", 1}};
}

DistanceType eucledianDist(const Company& c1, const Company& c2) {
  return std::sqrt(std::pow(c2.revenue - c1.revenue, 2)
                       + std::pow(c2.getNumericTeamCharacteristic() - c1.getNumericTeamCharacteristic(), 2));
}

DistanceType manhattanDist(const Company& c1, const Company& c2) {
  return std::abs(c2.revenue - c1.revenue)
      + std::abs(c2.getNumericTeamCharacteristic() - c1.getNumericTeamCharacteristic());
}

int main() {
  CompanyDataset dataset = getCompanyDataset();
  size_t k;
  std::cout << "Enter K: ";
  std::cin >> k;

  while (true) {
    std::cout << "Enter revenue(negative value to stop): ";
    long rev;
    std::cin >> rev;

    if (rev < 0) {
      break;
    }

    std::cout << "Enter team characteristic: ";
    std::string tc;
    std::cin >> tc;

    bool isSuccessful = classifyCompanySuccess(k, dataset, {static_cast<int>(rev), tc, 0}, manhattanDist);
    std::cout << isSuccessful << std::endl;
    dataset.push_back({static_cast<int>(rev), tc, isSuccessful});
  }

  return 0;
}
