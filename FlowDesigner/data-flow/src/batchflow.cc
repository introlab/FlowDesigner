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
    ParameterSet param;
    for (int arg = 2; arg<argc; arg++)
      {
	char arg_name[100];
	sprintf (arg_name, "ARG%d", arg-1);
	param.add(arg_name, ObjectRef (new String (argv[arg])));
      }
    UIDocument *doc = new UIDocument(argv[1]);
    doc->load();
    doc->run(param);
  }
  catch (BaseException *e) 
  {
     return 1;
     e->print();
  }  
  catch (...) {
    cerr<<"Unhandled exception in "<<argv[0]<<endl;
    cerr<<"Exiting"<<endl;
    return 1;
  }

  return 0;
}

