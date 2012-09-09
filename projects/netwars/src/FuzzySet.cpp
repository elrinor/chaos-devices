#include "config.h"
#include "FuzzySet.h"
#include <map>
using namespace std;

class FuzzySet::FuzzySetImpl {
private:
  map<int, float> set;

public:
  float& operator[] (int number) {
    return this->set[number];
  }

  const float& operator[] (int number) const {
    const static float tmp = 0.0f;
    map<int, float>::const_iterator i = this->set.find(number);
    if(i == this->set.end())
      return tmp;
    return i->second;
  }

  FuzzySetImpl* clone() {
    FuzzySetImpl* result = new FuzzySetImpl();
    result->set = this->set;
    return result;
  }

  void serialize(std::ostream& stream) const {
    stream << this->set.size();
    for(map<int, float>::const_iterator i = this->set.begin(); i != this->set.end(); i++) 
      stream << " " << i->first << " " << i->second;
  }
};

float& FuzzySet::operator[] (int number) {
  return (*this->impl)[number];
}

const float& FuzzySet::operator[] (int number) const {
  return (*this->impl)[number];
}

FuzzySet FuzzySet::clone() {
  return FuzzySet(this->impl->clone());
}

FuzzySet::FuzzySet(FuzzySetImpl* impl): impl(impl) {
}

FuzzySet::FuzzySet(): impl(new FuzzySetImpl()) {
}

FuzzySet FuzzySet::deserialize(std::istream& stream) {
  FuzzySet result;
  int size;
  stream >> size;
  for(int i = 0; i < size; i++) {
    int index;
    float value;
    stream >> index >> value;
    result[index] = value;
  }
  return result;
}

void FuzzySet::serialize(std::ostream& stream) const {
  this->impl->serialize(stream);
}
