// Copyright (C) 2001 Jean-Marc Valin


#include <vector>
#include <string>
#include "DLManager.h"
#include <sys/types.h>
#include <dirent.h>

vector<string> envList(char *envName, bool include_home)
{
   vector<string> list;
   if (include_home)
   {
      string prefix = INSTALL_PREFIX;
      char *home = getenv("VFLOW_HOME");
      if (home && strcmp(home, "")!=0)
	 prefix=home;
      list.insert(list.end(), prefix+ "/share/overflow/toolbox");
      list.insert(list.end(), prefix+ "/share/overflow/lib");
   }
   char *strPath = getenv(envName);
   if (!strPath)
      return list;
   string path = strPath; 
   int start = 0;
   unsigned int pos = 0;
   while (pos < path.length())
   {
      if (path[pos] == ':')
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



void scanDL(bool debug)
{
   vector<string> libList;
   
   if (debug)
      cerr << "Overflow loading all toolbox code (DL)" << endl;
#ifdef PIC
   vector<string> dirs=envList("VFLOW_PATH");
#else
   vector<string> dirs=envList("VFLOW_PATH", false);
#endif
   if (dirs.size() == 0)
   {
      cerr << "Cannot find any toolbox. Exiting\n";
      exit(1);
   }
   for (unsigned int i = 0; i<dirs.size();i++)
   {
      if (debug)
	 cerr << "scanDL: Looking in directory " << dirs[i] << endl;
      DIR *my_directory = opendir (dirs[i].c_str());
      if (!my_directory)
	 continue;
      struct dirent *current_entry;
      for (current_entry = readdir(my_directory); 
	   current_entry != NULL; current_entry = readdir(my_directory)) 
      {
	 if (!strstr(current_entry->d_name, ".tlb"))
	 {
	    //cerr << current_entry->d_name << " is not a shared library\n";
	    continue;
	 }
	 string fullname = dirs[i] + "/" + current_entry->d_name;
	 if (debug)
	    cerr << "Found " << fullname << endl;
	 libList.push_back(fullname);
	 //DLManager::getLib(fullname);
	 //_DL_OPEN(fullname.c_str());
      }
      
      closedir(my_directory);
   }
   vector<string> errors = ToolboxList::load(libList, debug);
   if (errors.size())
   {
      cerr << "There were errors loading the toolboxes:\n";
      for (unsigned int i=0;i<errors.size();i++)
	 cerr << errors[i] << endl;
   }
   if (debug)
      cerr << "DL Loading done." << endl;
   
}
