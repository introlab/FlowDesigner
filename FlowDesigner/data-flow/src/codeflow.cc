// Copyright (C) 2001 Jean-Marc Valin

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include "path.h"

int main(int argc, char **argv)
{

  try {
    scanDL();
    UIDocument::loadAllInfo();
    UIDocument *doc = new UIDocument(argv[1]);
    doc->load();
    ofstream out("code.cc");
    doc->genCode(out, "buildDoc");
  }
  catch (BaseException *e) 
  {
     e->print();
  }  
  catch (...) {
    cerr<<"Unhandled exception in "<<argv[0]<<endl;
    cerr<<"Exiting"<<endl;
    exit(-1);
  }

  return 0;
}

