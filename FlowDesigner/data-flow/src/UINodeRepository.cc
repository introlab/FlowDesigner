// Copyright (C) 2001 Jean-Marc Valin


#include "UINodeRepository.h"
#include "path.h"
#include <tree.h>
#include <parser.h>
#include "BaseException.h"

/*This bunch of includes is for searching directories, maybe there's a better way...*/
#include <sys/stat.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>


//@implements UIClasses

UINodeRepository &UINodeRepository::GlobalRepository()
{
   static UINodeRepository rep;
   return rep;
}

map<string, set<string> > &UINodeRepository::ModuleDepend()
{
   static map<string, set<string> > dep;
   return dep;
}
   
map<string, set<string> > &UINodeRepository::FileDepend()
{
   static map<string, set<string> > dep;
   return dep;
}

map<string, set<string> > &UINodeRepository::HeaderDepend()
{
   static map<string, set<string> > dep;
   return dep;
}

void UINodeRepository::Scan()
{
   vector<string> dirs = envList("VFLOW_PATH");
   
   for (unsigned int i=0;i<dirs.size();i++) 
   {
      loadAllInfoRecursive(dirs[i]);
   }
}

set<string> &UINodeRepository::FindFileFromModule(const string &name)
{
   return ModuleDepend()[name];
}

set<string> &UINodeRepository::FindModuleFromFile(const string &name)
{
   return FileDepend()[name];
}

set<string> &UINodeRepository::FindHeaderFromFile(const string &name)
{
   return HeaderDepend()[name];
}



NodeInfo *UINodeRepository::Find(const string &name)
{
   map<string, NodeInfo *>::iterator found = GlobalRepository().info.find(name);
   if (found != GlobalRepository().info.end())
      return found->second;
   return NULL;
}

vector<string> UINodeRepository::Available()
{
   vector<string> allNodes;
   string nextItem;
   map<string, NodeInfo *>::iterator iter = GlobalRepository().info.begin();
	
   while (iter != GlobalRepository().info.end()) {
      nextItem = string((*iter).second->category) + "***" + 
	 string((*iter).first);
      allNodes.insert(allNodes.end(), nextItem);
      iter++;
   }
   return allNodes;
}

NodeInfo *UINodeRepository::findNode(const string &name)
{
   map<string, NodeInfo *>::iterator found = info.find(name);
   if (found != info.end())
      return found->second;
   found = GlobalRepository().info.find(name);
   if (found != GlobalRepository().info.end())
      return found->second;
   return NULL;
}

void UINodeRepository::loadNodeDefInfo(const string &path, const string &name)
{

   string fullname = path + "/" + name;
   xmlDocPtr doc = xmlParseFile(fullname.c_str());
   
   if (!doc || !doc->root || !doc->root->name)
   {
      cerr << "loadNodeDefInfo: error loading " << fullname << "\n";
      xmlFreeDoc (doc);
      return;
   }
   xmlNodePtr root=doc->root;
   xmlNodePtr node = root->childs;
   while (node != NULL)
   {
      string nodeName;
      if (string((char*)node->name) == "NodeClass")
      {
	 NodeInfo *info = new NodeInfo;
	 info->category = string((char *)xmlGetProp(node, (CHAR *)"category"));
	 char *sfile = (char *)xmlGetProp(node, (CHAR *)"source");
	 if (sfile)
	    info->sourceFile= string(sfile);
	 char *req = (char *)xmlGetProp(node, (CHAR *)"require");
	 if (req)
	    info->requireList = string(req);
	 //cerr << info->sourceFile << ":" << info->requireList << endl;
	 nodeName = string((char *)xmlGetProp(node, (CHAR *)"name"));
	 GlobalRepository().info[nodeName] = info;
	 xmlNodePtr data = node->childs;
	 while (data != NULL)
	 {
	    string kind = string((char*)data->name);
	    if (kind == "Input")
	    {
	       xmlChar *tmp;
	       ItemInfo *newInfo = new ItemInfo;
	       newInfo->name = string((char *)xmlGetProp(data, (CHAR *)"name"));
	       newInfo->type = string((char *)xmlGetProp(data, (CHAR *)"type"));
	       
	       tmp = xmlGetProp(data, (CHAR *)"value");
	       if (tmp == NULL)
		  newInfo->value = "";
	       else
		  newInfo->value = string((char *)tmp);
	       
	       tmp = xmlNodeListGetString(doc, data->childs, 1);
	       if (tmp == NULL)
		  newInfo->description = "No Description Available.";
	       else
		  newInfo->description = string((char *)tmp);
	       
	       info->inputs.insert(info->inputs.end(), newInfo);
	    } else if (kind == "Output")
	    {
	       xmlChar *tmp;
	       ItemInfo *newInfo = new ItemInfo;
	       newInfo->name = string((char *)xmlGetProp(data, (CHAR *)"name"));
	       newInfo->type = string((char *)xmlGetProp(data, (CHAR *)"type"));
	       
	       tmp = xmlGetProp(data, (CHAR *)"value");
	       if (tmp == NULL)
		  newInfo->value = "";
	       else
		  newInfo->value = string((char *)tmp);
	       
	       tmp = xmlNodeListGetString(doc, data->childs, 1);
	       if (tmp == NULL)
		  newInfo->description = "No Description Available.";
	       else
		  newInfo->description = string((char *)tmp);
	       
	       info->outputs.insert(info->outputs.end(), newInfo);
	    } else if (kind == "Parameter")
	    {
	       xmlChar *tmp;
	       ItemInfo *newInfo = new ItemInfo;
	       newInfo->name = string((char *)xmlGetProp(data, (CHAR *)"name"));
	       newInfo->type = string((char *)xmlGetProp(data, (CHAR *)"type"));
	       tmp = xmlGetProp(data, (CHAR *)"value");
	       if (tmp == NULL)
		  newInfo->value = "";
	       else
		  newInfo->value = string((char *)tmp);
	       
	       tmp = xmlNodeListGetString(doc, data->childs, 1);
	       if (tmp == NULL)
		  newInfo->description = "No Description Available.";
	       else
		  newInfo->description = string((char *)tmp);
	       
	       info->params.insert(info->params.end(), newInfo);
	    } else if (kind == "Description")
	    {
	       xmlChar *tmp;
	       tmp = xmlNodeListGetString(doc, data->childs, 1);
	       if (tmp == NULL)
		  info->description = "No description available";
	       else
		  info->description = string((char *)tmp);
	       
	    } else 
	    {
	       cerr << "other\n";
	    }
	    data = data->next;
	 }
      }
      /*Dependencies for modules*/
      else if (string((char*)node->name) == "ModuleDepend")
      {
	 char *dep_module = (char *)xmlGetProp(node, (CHAR *)"module");
	 if (!dep_module)
	    throw new GeneralException("Empty module dependency", __FILE__, __LINE__);
	 xmlNodePtr depend = node->childs;
	 while (depend != NULL)
	 {
	    if (string((char*)depend->name) != "Require")
	       throw new GeneralException(string("Unknown section in module dependency: ") + (char*)node->name, 
				    __FILE__, __LINE__);

	    char *req_file = (char *)xmlGetProp(depend, (CHAR *)"file");
	    if (!req_file)
	       throw new GeneralException(string("Empty dependency for module: ") + dep_module, __FILE__, __LINE__);
	    
	    ModuleDepend()[dep_module].insert(ModuleDepend()[dep_module].end(), req_file);

	    depend = depend->next;
	 }
      } 
      /*Dependencies for files*/
      else if (string((char*)node->name) == "FileDepend")
      {
	 char *dep_file = (char *)xmlGetProp(node, (CHAR *)"file");
	 if (!dep_file)
	    throw new GeneralException("Empty file dependency", __FILE__, __LINE__);
	 xmlNodePtr depend = node->childs;
	 while (depend != NULL)
	 {
	    if (string((char*)depend->name) == "RequireModule")
	    {
	       char *req_module = (char *)xmlGetProp(depend, (CHAR *)"module");
	       if (!req_module)
		  throw new GeneralException(string("Empty module dependency for file: ") + dep_file, 
					     __FILE__, __LINE__);
	       FileDepend()[dep_file].insert(FileDepend()[dep_file].end(), req_module);
	    }
	    else if (string((char*)depend->name) == "RequireHeader")
	    {
	       char *req_header = (char *)xmlGetProp(depend, (CHAR *)"header");
	       if (!req_header)
		  throw new GeneralException(string("Empty header dependency for file: ") + dep_file, 
					     __FILE__, __LINE__);
	       HeaderDepend()[dep_file].insert(HeaderDepend()[dep_file].end(), req_header);	       
	    } else 
	       throw new GeneralException(string("Unknown section in file dependency: ") + (char*)node->name, 
				    __FILE__, __LINE__);

	    depend = depend->next;
	 }	 
      } 
      else 
	 throw new GeneralException(string("Unknown section in toolbox definition file: ") + (char*)node->name, 
				    __FILE__, __LINE__);
      
      node = node->next;
   }
   xmlFreeDoc(doc);
   
}

void UINodeRepository::loadExtDocInfo(const string &path, const string &name)
{
   string fullname = path + "/" + name;
   xmlDocPtr doc = xmlParseFile(fullname.c_str());
   string basename = string(name.begin(), name.end()-2);

   if (!doc || !doc->root || !doc->root->name)
   {
      cerr << "ExtDoc: error loading " << fullname << "\n";
      xmlFreeDoc (doc);
      return;
   }

   map<string, NodeInfo *> &externalDocInfo = GlobalRepository().info;

   if (externalDocInfo.find(basename) != externalDocInfo.end())
   {
      cerr << "error: net " << basename << " already existed\n";
      return;
   }
   //cerr << "new subnet info with name: " << netName << "\n";
   NodeInfo *info = new NodeInfo;
   externalDocInfo[basename] = info;
   

   xmlNodePtr root=doc->root;
   xmlNodePtr net = root->childs;
   
   while (net != NULL)
   {
      //cerr << "scanning networks...\n";
      if (string((char*)net->name) == "Network")
      {
	 //cerr << "scanning a net\n";
	 string netName = string((char *)xmlGetProp(net, (CHAR *)"name"));
	 if (netName == "MAIN")
	 {
	    
	    CHAR *category = xmlGetProp(net, (CHAR *)"category");
	    if (category)
	       info->category = string((char *)category);

	    //loadNetInfo(net, externalDocInfo, basename);
	   
 
	    xmlNodePtr node = net->childs;
	    while (node != NULL)
	    {
	       if (string((char*)node->name) == "NetInput")
	       {
		  string termName = string((char *)xmlGetProp(node, (CHAR *)"name"));
		  ItemInfo *newInfo = new ItemInfo;
		  newInfo->name = termName;
		  info->inputs.insert (info->inputs.end(), newInfo);
	 
	       } else if (string((char*)node->name) == "NetOutput")
	       {
		  string termName = string((char *)xmlGetProp(node, (CHAR *)"name"));
		  ItemInfo *newInfo = new ItemInfo;
		  newInfo->name = termName;
		  info->outputs.insert (info->outputs.end(), newInfo);
	 
	       }
	       node = node->next;
	    }


	 }
      } else if (string((char*)net->name) == "Parameter")
      {
	 char *param_name = (char *)xmlGetProp(net, (CHAR *)"name");
	 char *param_type = (char *)xmlGetProp(net, (CHAR *)"type");
	 char *param_value = (char *)xmlGetProp(net, (CHAR *)"value");
	 if (param_name && param_type)
	 {
	    ItemInfo *newInfo = new ItemInfo;
	    newInfo->name = param_name;
	    if (string(param_type)=="")
	       newInfo->type = "int";
	    else
	       newInfo->type = param_type;
	    if (string(param_value)!="")
	       newInfo->value = param_value;
	    info->params.insert (info->params.end(), newInfo);
	 }
	 //cerr << "param\n";
      }
      net = net->next;
   }
   xmlFreeDoc(doc);

}





void UINodeRepository::loadAllInfoRecursive(const string &path) {

  struct stat my_stat;
  DIR *my_directory = opendir (path.c_str());

  if (!my_directory) return;

  struct dirent *current_entry;

  for (current_entry = readdir(my_directory); 
       current_entry != NULL; current_entry = readdir(my_directory)) {
    
    string name = current_entry->d_name;
    string fullpath = path + string("/") + name;

    if (stat(fullpath.c_str(), &my_stat) < 0) {
       //cerr<<"stat error"<<endl;
       perror(fullpath.c_str());
      continue;
    }
    
    if (S_ISDIR(my_stat.st_mode)) {
      //it is a directory, let's doing it recursively
      if (name != string("..") && name != string(".")) {
	loadAllInfoRecursive(fullpath);
      }
    }
    else {
       
      //loading network
       int len = strlen(current_entry->d_name);
       if (len > 2 && strcmp(".n", current_entry->d_name + len-2)==0)
       {
	  //cout<<"Loading network : "<<fullpath<<endl;
	  loadExtDocInfo(path, name);
       }
       
       //loading toolbox
       if (len > 4 && strcmp(".def", current_entry->d_name + len-4)==0)
       {
	  //cout<<"Loading toolbox : "<<fullpath<<endl;
	  loadNodeDefInfo(path, name);
       }
       
    }
  }

  closedir(my_directory);
}






void UINodeRepository::ProcessDependencies(set<string> &initial_files, bool toplevel)
{
   int nbDepends = initial_files.size();

   //Process module/file dependencies, loop until there's nothing else to add
   do {
      nbDepends = initial_files.size();
      set<string> addMod;
      //Core is always necessary and we'll save a bunch of @require core
      addMod.insert(addMod.begin(), "core");

      //Scan all files in required list to find all required modules
      set<string>::iterator file=initial_files.begin();
      while (file != initial_files.end())
      {
	 if (FileDepend().find(*file) != FileDepend().end())
	 {
	    set<string> &moduleDep = FileDepend()[*file];
	    set<string>::iterator mod = moduleDep.begin();
	    while (mod != moduleDep.end())
	    {
	       addMod.insert(addMod.end(), *mod);
	       mod++;
	    }
	 }
	 file++;
      }

      //Scan all modules in required list to find all required files
      set<string>::iterator mod=addMod.begin();
      while (mod != addMod.end())
      {
	 if (ModuleDepend().find(*mod) != ModuleDepend().end())
	 {
	    set<string> &fileDep = ModuleDepend()[*mod];
	    set<string>::iterator file = fileDep.begin();
	    while (file != fileDep.end())
	    {
	       initial_files.insert(initial_files.end(), *file);
	       file++;
	    }
	 }
	 mod++;
      }

   } while (nbDepends != initial_files.size());

   //Repeat recursivly until there's nothing else to add
   //if (nbDepends != initial_files.size())
   //   processDependencies(initial_files, false);

   //Process header dependencies, loop until there's nothing else to add
   do {
      nbDepends = initial_files.size();
      if (toplevel)
      {
	 set<string>::iterator file=initial_files.begin();
	 while (file != initial_files.end())
	 {
	    if (HeaderDepend().find(*file) != HeaderDepend().end())
	    {
	       set<string> &headerDep = HeaderDepend()[*file];
	       set<string>::iterator header = headerDep.begin();
	       while (header != headerDep.end())
	       {
		  initial_files.insert(initial_files.end(), *header);
		  header++;
	       }
	    }
	    file++;
	 }
      
      }
   } while (nbDepends != initial_files.size());

}
