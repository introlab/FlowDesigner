// Copyright (C) 1999 Jean-Marc Valin

#include "DoubleDispatch.h"

//@implements core
using namespace std;

namespace FD {

DoubleDispatchException::DoubleDispatchException(DoubleDispatch *_table, string _type1, string _type2)
   : table(_table)
   , type1(_type1)
   , type2(_type2)
{
}

void DoubleDispatchException::print(ostream &out) 
{
  out << "DoubleDispatch Vtable error: no match for " << table->getName()<<" (" << type1 << ", " << type2 << ")" << endl;
}

std::map<std::string,DoubleDispatch> & DoubleDispatch::getAllTables() {
  static map<std::string,DoubleDispatch> allTables;
  return allTables;
}

DoubleDispatch& DoubleDispatch::getTable(const std::string &tableName) {
  //static map<std::string,DoubleDispatch> allTables;   
  return DoubleDispatch::getAllTables()[tableName];
}

//indirect way to get the name
std::string DoubleDispatch::getName()
{
  
  for (map<std::string,DoubleDispatch>::iterator iter = DoubleDispatch::getAllTables().begin();
       iter != DoubleDispatch::getAllTables().end(); iter++) 
  {
    if (&(iter->second) == this) {
      return iter->first;
    }
  }
  
  return string("unknown");
}

}//namespace FD
