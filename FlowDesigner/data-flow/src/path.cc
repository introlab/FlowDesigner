// Copyright (C) 2001 Jean-Marc Valin
#include <iostream>
#include <vector>
#include <string>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>

#ifndef WIN32
#include "DLManager.h"
#else
#warning "Dynamic loading of toolboxes disabled for WIN32"
#endif

using namespace std;

namespace FD {

vector<string> envList(const char *envName, bool include_home)
{
   vector<string> list;
   if (include_home)
   {
#ifndef WIN32
      string prefix = INSTALL_PREFIX;
      char *home = getenv("FLOWDESIGNER_HOME");
      if (home && strcmp(home, "")!=0)
	 prefix=home;

      list.insert(list.end(), prefix+ "/lib/flowdesigner/toolbox");
#else
	  #warning Hard coded prefix for debug purposes
	  cerr<<"INSTALL_PREFIX IS : "<<INSTALL_PREFIX<<endl;
	  list.insert(list.end(), INSTALL_PREFIX);
#endif
   }
   char *strPath = getenv(envName);
   if (!strPath)
      return list;
   string path = strPath; 
   int start = 0;
   unsigned int pos = 0;
   while (pos < path.length())
   {
#ifndef WIN32
      if (path[pos] == ':')
#else
	  if (path[pos] == ';')
#endif	  
      {
	 list.insert(list.end(), string(&(path[start]), &(path[pos])));
	 start = pos+1;
      }
      pos++;
   }
   if (pos)
      list.insert(list.end(), string(&(path[start]), &(path[pos])));

   //cerr << pathList << endl;
   return list;
}


//Added recursive scan to look for toolbox subdirectories
//(DL) 06/02/2004
void recursiveScanDL(const string &path, vector<string> &libList, bool debug) {
  
  if (debug)
    cerr<<"recursive DL scan with path : "<<path<<endl;
  
  DIR *my_directory = opendir (path.c_str());
  
  if (!my_directory) {
    perror((std::string("error opening directory ") + path).c_str());
    return;
  }
  
  struct dirent *current_entry;
  
  for (current_entry = readdir(my_directory); 
       current_entry != NULL; current_entry = readdir(my_directory)) {
      
    struct stat my_stat;
    string name = current_entry->d_name;
#ifndef WIN32
    string fullpath = path + "/" + name;
#else
	string fullpath = path + "\\" + name;
#endif
    
    //is it a directory, if so let's scan it...
    if (stat(fullpath.c_str(), &my_stat) < 0) {	    
      perror(fullpath.c_str());
      continue;
    }
    
    if (S_ISDIR(my_stat.st_mode)) {
      //it is a directory, let's doing it recursively
      if (name != string("..") && name != string(".")) {
	recursiveScanDL(fullpath, libList,debug);
      }
    }
    else {
      //this is a standard file, look for the .tlb extension
      if (name.find(".tlb") != string::npos) {	    
	if (debug) {
	  cerr << "Found " << fullpath << endl;
	}   
	libList.push_back(fullpath);
      }
    }       
  }//for all entries
  
  closedir(my_directory);
  
}


void scanDL(bool debug)
{
	
#ifndef WIN32
   vector<string> libList;
   
   if (debug) {
      cerr << "FlowDesigner loading all toolbox code (DL)" << endl;
   }

#ifdef PIC
   vector<string> dirs=envList("FLOWDESIGNER_PATH");
#else
   vector<string> dirs=envList("FLOWDESIGNER_PATH", false);
#endif

   if (dirs.size() == 0)
   {
      cerr << "Cannot find any toolbox. Returning\n";
      return;
	  //exit(1);
   }

   for (unsigned int i = 0; i<dirs.size();i++)
   {
     if (debug) {
       cerr << "scanDL: Looking in directory " << dirs[i] << endl;
     }
     
     //(DL) 06/02/2004
     //calling new recursive function to scan for toolboxes
     recursiveScanDL(dirs[i], libList, debug);
   }


   vector<string> errors = ToolboxList::load(libList, debug);
 
   if (errors.size())
   {
      cerr << "There were errors loading the toolboxes:\n";

      for (unsigned int i=0;i<errors.size();i++) 
	{
	  cerr << errors[i] << endl;
	}

   }   
   if (debug) {
      cerr << "DL Loading done." << endl;
   }
#endif   
}

}//namespace FD
