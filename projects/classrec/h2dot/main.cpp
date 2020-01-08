#include <iostream>
#include <string>
#include <map>
#include <set>
#include <fstream>
#include <boost/regex.hpp>

using namespace std;
using namespace boost;


map<int, string> lastInDepth;

map<string, set<string> > parents;

map<string, int> indexes;

set<string> reached;

int lastIndex = 0;

regex matcher;

int main(int argc, char** argv) {
  if(argc < 2) {
    cout << "USAGE: h2dot <filename> [regex]" << endl;
    return -1;
  } else {
    ifstream f(argv[1]);

    if(!f.is_open() || f.fail())
      return -1;

    while(!f.eof()) {
      string s;

      getline(f, s);

      if(s.size() < 8)
        continue;

      int depth = 0;
      if(s.substr(4, 2) == ": ") {
        s = s.substr(6);
        while(s.substr(0, 2) == "->") {
          depth++;
          s = s.substr(2);
        }

        if(indexes.find(s) == indexes.end())
          indexes[s] = lastIndex++;
        
        lastInDepth[depth] = s;
        if(depth != 0)
          parents[lastInDepth[depth - 1]].insert(s);
      }
    }

    if(argc <= 2)
      matcher = regex(".*");
    else
      matcher = regex(argv[2]);

    for(map<string, int>::iterator i = indexes.begin(); i != indexes.end(); i++)
      if(regex_match(i->first, matcher))
        reached.insert(i->first);

    while(true) {
      int ops = 0;
      for(map<string, set<string> >::iterator i = parents.begin(); i != parents.end(); i++) {
        for(set<string>::iterator j = i->second.begin(); j != i->second.end(); j++) {
          if(reached.find(i->first) != reached.end()) {
            if(reached.find(*j) == reached.end())
              ops++;
            reached.insert(*j);
          }
          if(reached.find(*j) != reached.end()) {
            if(reached.find(i->first) == reached.end())
              ops++;
            reached.insert(i->first);
          }
        }
      }

      if(ops == 0)
        break;
    }

    cout << "digraph G {" << endl;
    // size = "12,12";
    // ordering=out;

    cout << "graph [fontname=\"Arial\",fontsize=25,rankdir=\"BT\"];" << endl;
    cout << "node [shape=box,fontname=\"Arial\"];" << endl;
    cout << "edge [arrowhead=\"empty\",fontname=\"Arial\"];"  << endl;
    for(set<string>::iterator i = reached.begin(); i != reached.end(); i++) {
      cout << "n" << indexes[*i] << " [label=\"" << *i << "\"];" << endl;
      for(set<string>::iterator j = parents[*i].begin(); j != parents[*i].end(); j++)
        if(reached.find(*j) != reached.end())
          cout << "n" << indexes[*i] << " -> n" << indexes[*j] << ";" << endl;
    }

    cout << "}" << endl;

  }
}