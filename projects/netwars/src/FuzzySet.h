#ifndef __FUZZYSET_H__
#define __FUZZYSET_H__
#include <iostream>
#include "arx/smart_ptr.h"

class FuzzySet {
private:
  class FuzzySetImpl;
  arx::shared_ptr<FuzzySetImpl> impl;
  FuzzySet(FuzzySetImpl* impl);
public:
  float& operator[] (int number);
  const float& operator[] (int number) const;
  FuzzySet clone();
  FuzzySet();

  static FuzzySet deserialize(std::istream& stream);
  void serialize(std::ostream& stream) const;
};


#endif
