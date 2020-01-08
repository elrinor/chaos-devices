#define _CRT_SECURE_NO_DEPRECATE
#include "FastOps.h"
#include <iostream>
using namespace std;

int main()
{
  unsigned short a[16] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  unsigned short b[16] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

  cout << SAD4x4w(a, b, 8);

  cin >> a[0];
  return 0;
}