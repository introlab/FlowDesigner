#include "Vector.h"
#include "Object.h"
#include "net_types.h"
#include <string>
#include <iostream>
#include "Exception.h"
#include "operators.h"

using namespace FD;
using namespace std;

int main(int argc, char* argv[]) {

  cerr<<"Cygwin DoubleDispatch test starting  ... "<<endl;
  cerr<<"addVtable.getSize() = "<<DoubleDispatch::getTable("addVtable").getSize()<<endl;
  cerr<<"subVtable.getSize() = "<<DoubleDispatch::getTable("subVtable").getSize()<<endl;


  try {

    ObjectRef valueRef1(Int::alloc(1));
    ObjectRef valueRef2(Int::alloc(2));

    ObjectRef Result = valueRef1 + valueRef2;

    Result->printOn(cerr);

    

  }
  catch (BaseException *e) {
    e->print(cerr);
    delete e;
  }
  return 0;


}
