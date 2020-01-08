#include <string>
#include <iostream>
#include "Crec.h"

int main(int argc, char** argv) {
  crec::Crec c;

  c.updateFromFile("objtree.txt");
  // c.reconstructHierarchy("objtree.dot");
}