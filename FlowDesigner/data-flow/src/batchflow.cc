#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"

int main(int argc, char **argv)
{

  try {
    UIDocument::scanDL();
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
  catch (BaseException *e) {  
    e->print();
  }  
  catch (...) {
    cerr<<"Unhandled exception in "<<argv[0]<<endl;
    cerr<<"Exiting"<<endl;
    exit(-1);
  }

  return 0;
}

