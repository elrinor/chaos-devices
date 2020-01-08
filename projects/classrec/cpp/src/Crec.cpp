#include "Crec.h"
#include <map>
#include <iostream>
#include <QFile>
#include <QDomDocument>

#ifdef _MSC_VER
#  pragma comment(lib, "Ws2_32.lib")
#  ifdef _DEBUG
#    pragma comment(lib, "QtCored.lib")
#    pragma comment(lib, "QtXmld.lib")
#  else
#    pragma comment(lib, "QtCore.lib")
#    pragma comment(lib, "QtXml.lib")
#  endif
#endif

namespace crec {
  template<class T>
  T* getChecked(std::map<uintptr_t, T*>& map, uintptr_t index) {
    std::map<uintptr_t, T*>::iterator p = map.find(index);
    if(p == map.end()) {
      T* result = new T(index);
      map.insert(std::make_pair(index, result));
      return result;
    } else
      return p->second;
  }

  void Crec::updateFromFile(std::string fileName) {
    QDomDocument doc;
    QFile xmlFile(fileName.c_str());
    doc.setContent(&xmlFile);

    if(!xmlFile.isOpen())
      throw std::runtime_error("Cannot open file \"" + fileName + "\"");
    
    QDomElement node = doc.documentElement();
    while(node.nodeName() != "chrdump" || node.attribute("version") != "0.1") {
      node = node.nextSiblingElement();
      if(node.isNull())
        throw std::runtime_error("Wrong file format: <chrdump version=\"0.1\"> section expected.");
    }

    std::map<uintptr_t, VTable*> vtables;
    std::map<uintptr_t, Function*> functions;
    std::vector<std::pair<VTable*, VTable*> > lhds;

    for(node = node.firstChildElement(); !node.isNull(); node = node.nextSiblingElement()) {
      if(node.nodeName() == "vtentry") {
        getChecked(vtables, node.attribute("vt").toULong(NULL, 16))->
          addEntry(node.attribute("index").toInt(), getChecked(functions, node.attribute("calls").toULong(NULL, 16)), node.attribute("withOffset").toInt());
      } else if(node.nodeName() == "method") {
        getChecked(functions, node.attribute("addr").toULong(NULL, 16))->setType(node.attribute("type") == "SDD" ? Function::SDD : Function::NONE)->
          setArgSize(node.attribute("argsSize").toInt())->setLength(node.attribute("length").toInt());
      } else if(node.nodeName() == "classname") {
        getChecked(vtables, node.attribute("vt").toULong(NULL, 16))->setClassName(node.attribute("name").toStdString());
      } else if(node.nodeName() == "relation") {
        if(node.attribute("type") != "lhd")
          throw("Only lhd relations are supported.");
        lhds.push_back(std::make_pair(getChecked(vtables, node.attribute("lvt").toULong(NULL, 16)), getChecked(vtables, node.attribute("rvt").toULong(NULL, 16))));
      } else if(node.nodeName() == "purevirtual") {
        mPureVirtual = getChecked(functions, node.attribute("addr").toULong(NULL, 16));
      }
    }

    for(std::map<uintptr_t, VTable*>::iterator i = vtables.begin(); i != vtables.end(); i++) {
      i->second->setIndex(mVTables.size());
      mVTables.push_back(i->second);
    }

    for(std::map<uintptr_t, Function*>::iterator i = functions.begin(); i != functions.end(); i++)
      mFunctions.push_back(i->second);

    for(unsigned int i = 0; i < lhds.size(); i++)
      mRestrictions.push_back(Restriction(lhds[i].first->getIndex(), lhds[i].second->getIndex(), Relation::LHD));
  }

}

