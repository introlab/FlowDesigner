// Copyright (C) 2001 Jean-Marc Valin

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include "path.h"

int main(int argc, char **argv)
{
   if (argc!=4)
   {
      cerr << "usage: codeflow  <input XML file>  <output C++ source file>  <build function name>" << endl;
      exit(1);
   }
   
   try 
   {
      scanDL();
      UIDocument::loadAllInfo();
      UIDocument *doc = new UIDocument(argv[1]);
      doc->load();
      ofstream out(argv[2]);
      doc->genCode(out, argv[3]);
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

