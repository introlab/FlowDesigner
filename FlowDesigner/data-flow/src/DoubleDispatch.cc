// Copyright (C) 1999 Jean-Marc Valin

#include "DoubleDispatch.h"

//@implements core

DoubleDispatchException::DoubleDispatchException(DoubleDispatch *_table, string _type1, string _type2)
   : table(_table)
   , type1(_type1)
   , type2(_type2)
{
}

void DoubleDispatchException::print(ostream &out) 
{
   out << "DoubleDispatch Vtable error: no match for " << table->getName() << "(" << type1 << ", " << type2 << ")" << endl;
}



SingleDispatchException::SingleDispatchException(SingleDispatch *_table, string _type1)
   : table(_table)
   , type1(_type1) {

}

void SingleDispatchException::print(ostream &out) {
   out << "SingleDisptach Vtable error: no match for " << table->getName() << "(" << type1 << ")" << endl;
}
