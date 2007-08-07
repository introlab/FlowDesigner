// Copyright (C) 2001 Jean-Marc Valin


#include "UINodeRepository.h"
#include "path.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include "BaseException.h"
#include "UINetwork.h"

/*This bunch of includes is for searching directories, maybe there's a better way...*/
#include <sys/stat.h>

#ifndef WIN32
#include <dlfcn.h>
#endif

#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>



//@implements UIClasses

using namespace std;

namespace FD {


ostream& operator<< (ostream &out, const ItemInfo &iInfo) {
	out<<"\tname:  "<<iInfo.name<<endl;
	out<<"\ttype:  "<<iInfo.type<<endl;
	out<<"\tvalue: "<<iInfo.value<<endl;
	out<<"\tdesc:  "<<iInfo.description<<endl;
	return out;
}

ostream& operator<< (ostream & out, const NodeInfo &nInfo) {
 
  out<<"INPUTS:"<<endl;
  for (unsigned int i = 0; i < nInfo.inputs.size(); i++) {
    out<<*(nInfo.inputs[i]);
  }
  out<<"OUTPUTS:"<<endl;
  for (unsigned int i = 0; i < nInfo.outputs.size(); i++) {
    out<<*(nInfo.outputs[i]);
  }
  out<<"PARAMETERS:"<<endl;
  for (unsigned int i = 0; i < nInfo.params.size(); i++) {
    out<<*(nInfo.params[i]);
  }
  out<<"CATEGORY:"<<nInfo.category<<endl;
  out<<"DESCRIPTION:"<<nInfo.description<<endl;
  out<<"SOURCEFILE:"<<nInfo.sourceFile<<endl;
  out<<"REQUIRELIST:"<<nInfo.requireList<<endl;
  out<<"KIND:"<<nInfo.kind<<endl;
  return out;
}



UINodeRepository::UINodeRepository(const UINodeRepository &)
{
   throw new GeneralException("I wouldn't try copying a UINodeRepository if I were you", __FILE__, __LINE__);
}


UINodeRepository &UINodeRepository::GlobalRepository()
{
   //cerr<<"Getting global repository"<<endl;
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

  cerr<<"UINodeRepository::Scan()"<<endl;

  //(DL) 06/02/2004
  vector<string> dirs  = envList("FLOWDESIGNER_PATH");

  
   for (unsigned int i=0;i<dirs.size();i++) 
   {
	  cerr<<"Scanning def "<<dirs[i]<<endl;
      LoadAllInfoRecursive(dirs[i]);
   }
   
   cerr<<"done loading def files"<<endl;

 /*  
   cerr<<"+++repository contains after loading: "<<endl;
   for (map<string,NodeInfo*>::iterator iter = GlobalRepository().info.begin();
	iter != GlobalRepository().info.end(); iter++) {
     cerr<<"**************"<<iter->first<<"("<<iter->second<<")"<<endl;
     cerr<<*(iter->second)<<endl;
   }
   */

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

UINodeRepository::~UINodeRepository()
{
   clean();
}

void UINodeRepository::clean()
{

  /*
   cerr<<"+++repository contains before cleaning: "<<endl;
   for (map<string,NodeInfo*>::iterator iter = GlobalRepository().info.begin();
	iter != GlobalRepository().info.end(); iter++) {
     cerr<<"**************"<<iter->first<<"("<<iter->second<<")"<<endl;
     cerr<<*(iter->second)<<endl;
   }
  */

   while(!info.empty()) {

     iterator iter = info.begin();
     //cerr<<"deleting NodeInfo : "<<iter->first<<endl;
     if (iter->second) {
       //deleting NodeInfo
       delete iter->second;
     }
     info.erase(iter);
   }

}


void UINodeRepository::LoadNodeDefInfo(const string &path, const string &name)
{

   string fullname = path + "/" + name;
   //cerr<<"Loading def :"<<fullname<<endl;
   xmlDocPtr doc = xmlParseFile(fullname.c_str());
   
   if (!doc || !doc->children || !doc->children->name)
   {
      cerr << "LoadNodeDefInfo: error loading " << fullname << "\n";
      xmlFreeDoc (doc);
      return;
   }
   xmlNodePtr root=doc->children;
   xmlNodePtr node = root->children;
   while (node != NULL)
   {
      string nodeName;
      if (string((char*)node->name) == "NodeClass")
      {
	 NodeInfo *my_info = new NodeInfo;
	 my_info->kind=NodeInfo::builtin;
	 char *str_category = (char *)xmlGetProp(node, (xmlChar *)"category");
	 if (str_category)
	    my_info->category = string(str_category);
	 else
	    my_info->category = "Unknown";
	 free(str_category);
	 char *sfile = (char *)xmlGetProp(node, (xmlChar *)"source");
	 if (sfile)
	    my_info->sourceFile= string(sfile);
	 free(sfile);
	 char *req = (char *)xmlGetProp(node, (xmlChar *)"require");
	 if (req)
	    my_info->requireList = string(req);
	 free(req);
	 //cerr << my_info->sourceFile << ":" << my_info->requireList << endl;
	 nodeName = string((char *)xmlGetProp(node, (xmlChar *)"name"));
	 //cerr<<"inserting into global repository  :"<<nodeName<<endl;
	 GlobalRepository().info[nodeName] = my_info;
	 
	 xmlNodePtr data = node->children;
	 while (data != NULL)
	 {
	    string kind = string((char*)data->name);
	    if (kind == "Input")
	    {
	       xmlChar *tmp;
	       ItemInfo *newInfo = new ItemInfo;
	       char *str_name = (char *)xmlGetProp(data, (xmlChar *)"name");
	       char *str_type = (char *)xmlGetProp(data, (xmlChar *)"type");

	       if (str_name) {
		 newInfo->name = string(str_name);
		 free(str_name);
	       }
	       if (str_type) {
		 newInfo->type = string(str_type);
		 free(str_type);
	       }
	       tmp = xmlGetProp(data, (xmlChar *)"value");
	       if (tmp == NULL)
		  newInfo->value = "";
	       else
		  newInfo->value = string((char *)tmp);
	       
	       tmp = xmlNodeListGetString(doc, data->children, 1);
	       if (tmp == NULL)
		  newInfo->description = "No Description Available.";
	       else
		  newInfo->description = string((char *)tmp);
	       
	       my_info->inputs.insert(my_info->inputs.end(), newInfo);
	    } else if (kind == "Output")
	    {
	       xmlChar *tmp;
	       ItemInfo *newInfo = new ItemInfo;
	       char *str_name = (char *)xmlGetProp(data, (xmlChar *)"name");
	       char *str_type = (char *)xmlGetProp(data, (xmlChar *)"type");
	       //FIXME: Check for NULL
	       newInfo->name = string(str_name);
	       newInfo->type = string(str_type);
	       free(str_name); free(str_type);
	       
	       tmp = xmlGetProp(data, (xmlChar *)"value");
	       if (tmp == NULL)
		  newInfo->value = "";
	       else
		  newInfo->value = string((char *)tmp);
	       
	       tmp = xmlNodeListGetString(doc, data->children, 1);
	       if (tmp == NULL)
		  newInfo->description = "No Description Available.";
	       else
		  newInfo->description = string((char *)tmp);
	       
	       my_info->outputs.insert(my_info->outputs.end(), newInfo);
	    } else if (kind == "Parameter")
	    {
	       xmlChar *tmp;
	       ItemInfo *newInfo = new ItemInfo;
	       char *str_name = (char *)xmlGetProp(data, (xmlChar *)"name");
	       char *str_type = (char *)xmlGetProp(data, (xmlChar *)"type");
	       
	       if (str_name) {
		 newInfo->name = string(str_name);
		 free(str_name);
	       }
	       if (str_type) {
		 newInfo->type = string(str_type);
		 free(str_type);
	       }
	      
	       tmp = xmlGetProp(data, (xmlChar *)"value");
	       if (tmp == NULL)
		  newInfo->value = "";
	       else
		  newInfo->value = string((char *)tmp);
	       free(tmp);
	       tmp = xmlNodeListGetString(doc, data->children, 1);
	       if (tmp == NULL)
		  newInfo->description = "No Description Available.";
	       else
		  newInfo->description = string((char *)tmp);
	       free(tmp);
	       my_info->params.insert(my_info->params.end(), newInfo);
	    } else if (kind == "Description")
	    {
	       xmlChar *tmp;
	       tmp = xmlNodeListGetString(doc, data->children, 1);
	       if (tmp == NULL)
		  my_info->description = "No description available";
	       else
		  my_info->description = string((char *)tmp);
	       free(tmp);
	    } else if (!xmlIsBlankNode(data))
	    {
	       cerr << "other\n";
	    }
	    data = data->next;
	 }
      }
      /*Dependencies for modules*/
      else if (string((char*)node->name) == "ModuleDepend")
      {
	 char *dep_module = (char *)xmlGetProp(node, (xmlChar *)"module");
	 if (!dep_module)
	    throw new GeneralException("Empty module dependency", __FILE__, __LINE__);
	 xmlNodePtr depend = node->children;
	 while (depend != NULL)
	 {
            if (xmlIsBlankNode(depend))
            {
               depend = depend->next;
               continue;
            }
	    if (string((char*)depend->name) != "Require")
	       throw new GeneralException(string("Unknown section in module dependency: ") + (char*)node->name, 
				    __FILE__, __LINE__);

	    char *req_file = (char *)xmlGetProp(depend, (xmlChar *)"file");
	    if (!req_file)
	       throw new GeneralException(string("Empty dependency for module: ") + dep_module, __FILE__, __LINE__);
	    
	    ModuleDepend()[dep_module].insert(ModuleDepend()[dep_module].end(), req_file);

	    depend = depend->next;
	 }
      } 
      /*Dependencies for files*/
      else if (string((char*)node->name) == "FileDepend")
      {
	 char *dep_file = (char *)xmlGetProp(node, (xmlChar *)"file");
	 if (!dep_file)
	    throw new GeneralException("Empty file dependency", __FILE__, __LINE__);
	 xmlNodePtr depend = node->children;
	 while (depend != NULL)
	 {
	    if (string((char*)depend->name) == "RequireModule")
	    {
	       char *req_module = (char *)xmlGetProp(depend, (xmlChar *)"module");
	       if (!req_module)
		  throw new GeneralException(string("Empty module dependency for file: ") + dep_file, 
					     __FILE__, __LINE__);
	       FileDepend()[dep_file].insert(FileDepend()[dep_file].end(), req_module);
	    }
	    else if (string((char*)depend->name) == "RequireHeader")
	    {
	       char *req_header = (char *)xmlGetProp(depend, (xmlChar *)"header");
	       if (!req_header)
		  throw new GeneralException(string("Empty header dependency for file: ") + dep_file, 
					     __FILE__, __LINE__);
	       HeaderDepend()[dep_file].insert(HeaderDepend()[dep_file].end(), req_header);	       
	    } else if (!xmlIsBlankNode(depend))
	       throw new GeneralException(string("Unknown section in file dependency: ") + (char*)node->name, 
				    __FILE__, __LINE__);

	    depend = depend->next;
	 }	 
      } 
      else if (!xmlIsBlankNode(node))
	 throw new GeneralException(string("Unknown section in toolbox definition file: ") + string((char*)node->name) + " in " + fullname, 
				    __FILE__, __LINE__);
      
      node = node->next;
   }
   xmlFreeDoc(doc);
   
}

void UINodeRepository::LoadExtDocInfo(const string &path, const string &name)
{
   string fullpath = path + "/" + name;
   string basename = string(name.begin(), name.end()-2);


   ifstream docFile(fullpath.c_str());
   if (docFile.fail())
   {
      cerr << "load: error loading " << fullpath << "\n";
      return;
   }
   char ch;
   docFile >> ch;
   if (ch=='#') 
   {
      while (ch != '<')
      {
	 docFile >> ch;
	 if (docFile.fail())
	 {
	    cerr << "ERROR\n";
	    return;
	 }
      }
   }
   docFile.putback(ch);
   string docStr;
   while(1)
   {
      //char buff[1025];
      //docFile.read(buff, 1024);
      //buff[1024]=0;
      string buff;
      getline( docFile, buff );
      if (docFile.fail())
      {
	 //docStr.append(buff, docFile.gcount());
	 docStr.append(buff.c_str(), docFile.gcount());
	 break; 
      }
      //docStr.append(buff, 1024);
      docStr.append(buff.c_str());
   }

   xmlDocPtr doc = xmlParseMemory (const_cast<char *> (docStr.c_str()), docStr.size());
   //xmlDocPtr doc = xmlParseFile(fullname.c_str());

   if (!doc || !doc->children || !doc->children->name)
   {
      cerr << "ExtDoc: error loading " << fullpath << "\n";
      xmlFreeDoc (doc);
      return;
   }

   GlobalRepository().loadDocInfo(doc, basename);
}

void UINodeRepository::loadDocInfo(xmlDocPtr doc, const string &basename)
{

   map<string, NodeInfo *> &externalDocInfo = GlobalRepository().info;

   if (externalDocInfo.find(basename) != externalDocInfo.end())
   {
      cerr << "error: net " << basename << " already existed\n";
      return;
   }
   //cerr << "new subnet info with name: " << netName << "\n";
   NodeInfo *my_info = new NodeInfo;
   my_info->kind = NodeInfo::external;

   //cerr<<"Inserting external info : "<<basename<<endl;
   externalDocInfo[basename] = my_info;
   

   xmlNodePtr root=doc->children;

   xmlChar *category = xmlGetProp(root, (xmlChar *)"category");
   if (category)
   {
      my_info->category = string((char *)category);
      free (category);
   }

   xmlNodePtr net = root->children;
   
   while (net != NULL)
   {
      //cerr << "scanning networks...\n";
      if (string((char*)net->name) == "Network")
      {
	 //cerr << "scanning a net\n";
	 string netName = string((char *)xmlGetProp(net, (xmlChar *)"name"));
	 if (netName == "MAIN")
	 {
	    

	    //loadNetInfo(net, externalDocInfo, basename);
	   
 
	    xmlNodePtr node = net->children;
	    while (node != NULL)
	    {
	       if (string((char*)node->name) == "NetInput")
	       {
		  string termName = string((char *)xmlGetProp(node, (xmlChar *)"name"));
		  ItemInfo *newInfo = new ItemInfo;
		  newInfo->name = termName;
		  my_info->inputs.insert (my_info->inputs.end(), newInfo);
	 
	       } else if (string((char*)node->name) == "NetOutput")
	       {
		  string termName = string((char *)xmlGetProp(node, (xmlChar *)"name"));
		  ItemInfo *newInfo = new ItemInfo;
		  newInfo->name = termName;
		  my_info->outputs.insert (my_info->outputs.end(), newInfo);
	 
	       }
	       node = node->next;
	    }


	 }
      } else if (string((char*)net->name) == "Parameter")
      {
	 char *param_name = (char *)xmlGetProp(net, (xmlChar *)"name");
	 char *param_type = (char *)xmlGetProp(net, (xmlChar *)"type");
	 char *param_value = (char *)xmlGetProp(net, (xmlChar *)"value");
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
	    my_info->params.insert (my_info->params.end(), newInfo);
	 }
	 //cerr << "param\n";
      }
      net = net->next;
   }
   xmlFreeDoc(doc);

}





void UINodeRepository::LoadAllInfoRecursive(const string &path) {

  struct stat my_stat;
  DIR *my_directory = opendir (path.c_str());

  if (!my_directory) return;

  struct dirent *current_entry;

  for (current_entry = readdir(my_directory); 
       current_entry != NULL; current_entry = readdir(my_directory)) {
    
    string name = current_entry->d_name;
#ifndef WIN32	
    string fullpath = path + string("/") + name;
#else
	string fullpath = path + string("\\") + name;
#warning Please remove this debug line.
	cerr<<"UINodeRepository::LoadAllInfoRecursive with full path : "<<fullpath<<endl;
#endif




    if (stat(fullpath.c_str(), &my_stat) < 0) {
       //cerr<<"stat error"<<endl;
       perror(fullpath.c_str());
      continue;
    }
    
    if (S_ISDIR(my_stat.st_mode)) {
      //it is a directory, let's doing it recursively
      if (name != string("..") && name != string(".")) {
	LoadAllInfoRecursive(fullpath);
      }
    }
    else {
       
      //loading network
       int len = strlen(current_entry->d_name);
       if (len > 2 && strcmp(".n", current_entry->d_name + len-2)==0)
       {
	  //cerr<<"Loading network : "<<fullpath<<endl;
	  LoadExtDocInfo(path, name);
       }
       
       //loading toolbox
       if (len > 4 && strcmp(".def", current_entry->d_name + len-4)==0)
       {
#ifdef WIN32       	
#warning Please remove this debug line.
	  cerr<<"Loading toolbox : "<<fullpath<<endl;
#endif
	  LoadNodeDefInfo(path, name);
       }
       
    }
  }

  closedir(my_directory);
}



void UINodeRepository::loadAllSubnetInfo(xmlNodePtr net)
{
   while (net != NULL)
   {
      if (string((char*)net->name) == "Network")
      {
	 
	 loadNetInfo(net);
	 
      }
      net = net->next;
   }
}


void UINodeRepository::loadNetInfo(xmlNodePtr net)
{
   char *str_netName = (char *)xmlGetProp(net, (xmlChar *)"name");
   string netName = string(str_netName);

   free(str_netName);
   xmlChar *category = xmlGetProp(net, (xmlChar *)"category");
   
   if (info.find(netName) != info.end())
   {
      cerr << "error: net " << netName << " already existed\n";
      return;
   }
   //cerr << "new subnet info with name: " << netName << "\n";
   NodeInfo *ninfo = new NodeInfo;
   ninfo->kind = NodeInfo::subnet;

   //we are dealing with a local repository, in a document
   info[netName] = ninfo;

      
   if (category)
   {
      ninfo->category = string((char *)category);
      free(category);
   }
   //cerr << "scan all nodes\n";
   xmlNodePtr node = net->children;
   while (node != NULL)
   {
      if (string((char*)node->name) == "Node")
      {	 	 
	 xmlNodePtr par = node->children;
	 //cerr << "par = " << par << endl;
	 while (par)
	 {
	    if (string((char*)par->name) == "Parameter")
	    {
	       char *str_paramName = (char *) xmlGetProp(par, (xmlChar *)"value");
	       char *str_type = (char *) xmlGetProp(par, (xmlChar *)"type");
	       string paramName = string (str_paramName);
	       string type = string (str_type);
	       free(str_paramName); free(str_type);
	       if (type == "subnet_param")
	       {
		  bool alreadyPresent = false;
		  for (unsigned int j=0;j<ninfo->params.size();j++)
		     if (ninfo->params[j]->name == paramName)
			alreadyPresent=true;
		  if (!alreadyPresent)
		  {
		     ItemInfo *newInfo = new ItemInfo;
		     newInfo->name = paramName;
		     //FIXME: This always sets the type to subnet_info, which is incorrect
		     newInfo->type = type;
		     ninfo->params.insert (ninfo->params.end(), newInfo);
		  }
	       }          
	    }
	    par = par->next;
	    
	 }
	 
	 
	 
	 //check for params
      }
      node = node->next;
   }
 

   //checking for the network type, we are interested in
   //iterators and threaded iterators here, nothing to do with subnets
   xmlChar *type = xmlGetProp(net, (xmlChar *)"type");
   if (type)
   { 
      if (string((char*)type)=="iterator")
      {
	 ItemInfo *newInfo = new ItemInfo;
	 newInfo->name = "DOWHILE";
	 newInfo->type = "bool";
	 ninfo->params.insert (ninfo->params.end(), newInfo);
	 free(type);
      } else if (string((char*)type)=="threaded")
      {
	 ItemInfo *newInfo = new ItemInfo;
	 newInfo->name = "RATE_PER_SECOND";
	 newInfo->type = "int";
	 ninfo->params.insert (ninfo->params.end(), newInfo);
	 free(type);	 
      }
   }


   //check for network description
   char *desc = (char *) xmlGetProp(net, (xmlChar *)"description");
   if (desc) {
     
     //set description
     ninfo->description = string(desc);

     //free XML string
     free(desc);
   }


  
   //scan all net inputs/outputs
   node = net->children;
   while (node != NULL)
   {
      if (string((char*)node->name) == "NetInput")
      {
	 char *str_name = (char *)xmlGetProp(node, (xmlChar *)"name");
	 char *str_type = (char *)xmlGetProp(node, (xmlChar *)"object_type");
	 char *str_desc = (char *)xmlGetProp(node, (xmlChar *)"description");

	 string termName = string(str_name);
	 string termType = "any";
	 string termDescription = "No description available";

	 //Free XML strings
	 free(str_name);
	 if (str_type) {
	   termType = str_type;
	   free(str_type);
	 }	 
	 if (str_desc) {
	   termDescription = str_desc;
	   free(str_desc);
	 }

	 ItemInfo *newInfo = new ItemInfo;
	 newInfo->name = termName;
	 newInfo->type = termType;
	 newInfo->description = termDescription;

	 ninfo->inputs.insert (ninfo->inputs.end(), newInfo);
	 
      } else if (string((char*)node->name) == "NetOutput")
      {
	 char *str_name = (char *)xmlGetProp(node, (xmlChar *)"name");
	 char *str_type = (char *)xmlGetProp(node, (xmlChar *)"object_type");
	 char *str_desc = (char *)xmlGetProp(node, (xmlChar *)"description");

	 string termName = string(str_name);
	 string termType = "any";
	 string termDescription = "No description available";

	 free(str_name);
	 if (str_type) {
	   termType = str_type;
	   free(str_type);
	 }	 
	 if (str_desc) {
	   termDescription = str_desc;
	   free(str_desc);
	 }

	 ItemInfo *newInfo = new ItemInfo;
	 newInfo->name = termName;
	 newInfo->type = termType;
	 newInfo->description = termDescription;

	 ninfo->outputs.insert (ninfo->outputs.end(), newInfo);
	 
      }
      node = node->next;
   }
 
}



void UINodeRepository::ProcessDependencies(set<string> &initial_files, bool toplevel)
{
   unsigned int nbDepends = initial_files.size();

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


void UINodeRepository::updateNetInfo(UINetwork *net)
{
  iterator inet = info.find(net->getName());
  if (inet!=info.end())
   {
     //cerr<<"UINodeRepository deleting network info:"<<net->getName()<<endl;
     delete inet->second;
   }

   NodeInfo *ninfo = new NodeInfo;
   vector<UINetTerminal *> my_terminals = net->getTerminals();

   //updating inputs & outputs
   //new implementation using name + type + description for all UINetTerminals
   //(DL) 15/12/203
   for (unsigned int i = 0; i < my_terminals.size(); i++)
   {
      
      ItemInfo *newInfo = new ItemInfo;

      if (my_terminals[i]) {

	//input name
	newInfo->name = my_terminals[i]->getName();

	//input type
	newInfo->type = my_terminals[i]->getType();
	  
	//input description 
	newInfo->description = my_terminals[i]->getDescription();
	  	  	  
	//input our output?
	if (my_terminals[i]->getType() == UINetTerminal::INPUT) {
	  ninfo->inputs.push_back(newInfo);
	}
	else if (my_terminals[i]->getType() == UINetTerminal::OUTPUT) {
	  ninfo->outputs.push_back(newInfo);
	}
	
      }
   }

   //cerr<<"insertingNetParams"<<endl;
   net->insertNetParams(ninfo->params);

   ninfo->category = "Subnet";

   //Description is now stored in a network definition
   ninfo->description = net->getDescription();
   
   //cerr<<"updated network info for "<<net->getName()<<endl;
   //we are dealing with a local repository, in a document
   info[net->getName()] = ninfo;
}

}//namespace FD
