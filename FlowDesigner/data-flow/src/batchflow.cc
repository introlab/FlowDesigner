// Copyright (C) 2001 Jean-Marc Valin

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include "path.h"
#include "Network.h"
#include "FlowException.h"
#include "iextensions.h"
#include "UINodeRepository.h"

int main(int argc, char **argv)
{
   try {
      if (argc < 2)
      {
	 cerr << "usage: batchflow <document> [arguments]" << endl;
	 exit(1);
      }
      IExtensions::detect();
      scanDL();
      UIDocument::loadAllInfo();
      UINodeRepository::Scan();
      ParameterSet param;
      for (int arg = 2; arg<argc; arg++)
      {
	 char arg_name[100];
	 sprintf (arg_name, "ARG%d", arg-1);
	 param.add(arg_name, ObjectRef (new String (argv[arg])));
      }
      UIDocument *doc = new UIDocument(argv[1]);
      doc->load();
      
      Network *net = doc->build("MAIN", param);
      if (net->getInputNode())
	 throw new GeneralException ("main network has input node", __FILE__, __LINE__);
      //cerr << "initializing...\n";
      net->initialize();
      //cerr << "running (UIDocument)...\n";
      for (int i = 0; ;i++) 
      {
	 if (!net->hasOutput(i)) 
	    break;
	 *net->getOutput(i,0);
      }
      
      
      //doc->run(param);
   }
   catch (BaseException *e) 
   {
      e->print();
      cerr << endl;
      return 1;
   }  
   catch (RCPtr<FlowException> e) 
   {
      cerr << "Unhandled FlowException: " << endl;
      e->printOn(cerr);
      cerr << endl;
      return 1;
   }  
   catch (exception e) {
      cerr << e.what() << endl;
      return 2;
   }
   catch (...) {
      cerr<<"Unknown unhandled exception in "<<argv[1]<<endl;
      cerr<<"Exiting"<<endl;
      return 2;
   }
   
   return 0;
}

