// Copyright (C) 2001 Jean-Marc Valin

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include "path.h"
#include <set>

using namespace std;

namespace FD {

int main(int argc, char **argv)
{
   if (argc!=4)
   {
      cerr << "usage: codeflow  <input XML file>  <output C++ source file>  <build function name>" << endl;
      exit(1);
   }
   cerr << "Warning: automatic code generation is in a very experimental stage right now\n";
   
   
   try 
   {
      scanDL();
      //UIDocument::loadAllInfo();
      UINodeRepository::Scan();

      UIDocument *doc = new UIDocument(argv[1]);
      doc->load();
      ofstream out(argv[2]);
      set<string> nodeList = doc->genCode(out, argv[3]);

      set<string> fileList;
      for (set<string>::iterator it=nodeList.begin();it!=nodeList.end();it++)
      {
	 NodeInfo *info = UINodeRepository::Find(*it);
	 if (info)
	    fileList.insert(fileList.begin(), info->sourceFile);
	 //fileList.insert(fileList.begin(), UIDocument::externalDocInfo[*it]->sourceFile);
      }
      UINodeRepository::ProcessDependencies(fileList);

      cerr << "dependencies:\n";
      for (set<string>::iterator it=fileList.begin();it!=fileList.end();it++)
	 cerr << *it << endl;
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


}//namespace FD
