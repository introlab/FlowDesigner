// Copyright (C) 2001 Jean-Marc Valin

#include "path.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include "UIDocument.h"
#include "UINetwork.h"
#include "UINode.h"
#include "Node.h"
#include "Network.h"
#include "ParameterSet.h"
#include <sys/stat.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <sstream>
#include <fstream>
#include <fcntl.h>
#include "stream_wrap.h"

//@implements UIClasses

UIDocument::UIDocument(string _name)
   : modified(false)
   , docName(_name)
   , untitled(true)
   , destroyed(false)
{
}

UIDocument::~UIDocument()
{
   if (!destroyed)
   {
      //cerr << "destroying UIDocument " << name << endl;
      for (unsigned int i=0;i<networks.size();i++)
      {
         delete networks[i];
         networks[i]=NULL;
      }
      
      for (unsigned int i=0;i<textParams.size();i++)
         delete textParams[i];
      
      for (unsigned int i=0;i<docInputs.size();i++)
         delete docInputs[i];
      
      for (unsigned int i=0;i<docOutputs.size();i++)
         delete docOutputs[i];
      
      for (unsigned int i=0;i<docParams.size();i++)
         delete docParams[i];
      destroyed=true;
   }
}


UINetwork *UIDocument::getNetworkNamed(const string &n)
{
   for (unsigned int i=0;i<networks.size();i++)
   {
      if (networks[i]->getName() == n)
         return networks[i];
   }
   return NULL;
}

vector<ItemInfo *> UIDocument::getNetInputs(const string &netName)
{
   //updateAllNetworks();
   vector <ItemInfo *> inputs;
   if (subnetInfo.findNode(netName))
      return subnetInfo.findNode(netName)->inputs;

   return inputs;
}

vector<ItemInfo *> UIDocument::getNetOutputs(const string &netName)
{
   //updateAllNetworks();
   vector <ItemInfo *> outputs;
   if (subnetInfo.findNode(netName))
      return subnetInfo.findNode(netName)->outputs;

   return outputs;
}

vector<ItemInfo *> UIDocument::getNetParams(const string &netName)
{
   //updateAllNetworks();
   vector <ItemInfo *> params;
   if (subnetInfo.findNode(netName))
      return subnetInfo.findNode(netName)->params;

   return params;
}

string UIDocument::getDescription(const string &type)
{
   NodeInfo *info = UINodeRepository::Find(type);
   if (info)
      return info->description;
   else
      return "Description not available";
}


void UIDocument::addParameterText(string name, string value, string type)
{
    DocParameterDataText *textInfo = new DocParameterDataText;
    textInfo->name = name;
    textInfo->value = value;
    textInfo->type = type;
    textParams.insert(textParams.end(), textInfo);
}

void UIDocument::printOn(ostream &out) const
{
   out << "<UIDocument" << endl;
   out << "<name " << docName << " >" << endl;
   out << ">" << endl;
}


void UIDocument::load()
{
   string fullpath=path+docName;
/*  This allows making scripts by ignoring the #! line at the beginning
    Unfortunately, it is incompatible with the compression feature of libXML*/

   //ostringstream docText;
   ifstream docFile(fullpath.c_str());
   if (docFile.fail())
   {
      error("Error: cannot open file");
      //cerr << "load: error loading " << fullpath << "\n";
      addNetwork("MAIN", UINetwork::subnet);
      resetModified();
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
	    error("Error: this doesn't look like an Overflow document");
	    addNetwork("MAIN", UINetwork::subnet);
	    resetModified();
	    return;
	 }
      }
   } else if (ch!='<')
   {
      error("Error: this doesn't look like an Overflow document");
      addNetwork("MAIN", UINetwork::subnet);
      resetModified();
      return;
   }
   string xmlStr;
   docFile >> xmlStr;
   if (xmlStr != "?xml")
   {
      error("Error: this doesn't look like an Overflow document");
      addNetwork("MAIN", UINetwork::subnet);
      resetModified();
      return;      
   }
   //docFile.putback(ch);
   string docStr="<?xml";
   while(1)
   {
      char buff[1025];
      docFile.read(buff, 1024);
      buff[1024]=0;
      if (docFile.fail())
      {
	 docStr.append(buff, docFile.gcount());
	 break; 
      }
      docStr.append(buff, 1024);
   }
   loadFromMemory(docStr.c_str(), docStr.size());

}

void UIDocument::loadFromMemory(const char *mem, int size)
{
   xmlDocPtr doc = xmlParseMemory (const_cast<char *> (mem), size);
   if (!doc || !doc->children || !doc->children->name)
   {
      error("Error: corrupted XML in file");
      addNetwork("MAIN", UINetwork::subnet);
      resetModified();      
      return;
   }
   xmlNodePtr root=doc->children;
   loadXML(root);
   xmlFreeDoc(doc);

}

void UIDocument::loadXML(xmlNodePtr root)
{
   //loadAllSubnetInfo(root->children);
   subnetInfo.clean();
   subnetInfo.loadAllSubnetInfo(root->children);

   //loading category if it exists
   xmlChar *cat = xmlGetProp(root, (xmlChar *)"category");
   if (cat)
   {
      category = string((char *)cat);
      free (cat);
   }

   //loading comments if they exists
   xmlChar *comments = xmlGetProp(root, (xmlChar *)"comments");
   if (comments)
   {
      m_comments = string((char *)comments);
      free (comments);
   }

   xmlNodePtr net = root->children;
   //cerr << "parsing...\n";
   while (net != NULL)
   {
      //cerr << "start net\n";
      if (string((char*)net->name) == "Network")
	addNetwork (net);
      net = net->next;
   }

  
   vector<ItemInfo *> tmp = getNetParams("MAIN");
   //cerr << "Got " << tmp.size() << " params in GUIDocument::createParamDialog\n";
   //textParams.resize(tmp.size());
   for (unsigned int i=0;i<tmp.size();i++)
     {   
        DocParameterDataText *newParam = new DocParameterDataText;
        newParam->name = string (tmp[i]->name);
        textParams.insert(textParams.end(), newParam);
        //textParams[i]->name=tmp[i];
     }
   //cerr << "--\n";
   xmlNodePtr par = root->children;
   //cerr << "par = " << par << endl;
   while (par)
   {
      if (string((char*)par->name) == "Parameter")
      {
	 char *str_name = (char *) xmlGetProp(par, (xmlChar *)"name");
	 char *str_type = (char *) xmlGetProp(par, (xmlChar *)"type");
	 char *str_value = (char *) xmlGetProp(par, (xmlChar *)"value");
         string name = string (str_name);
         string type = string (str_type);
         string value = string (str_value);
	 free(str_name); free(str_type); free(str_value);
         
         for (unsigned int i=0;i<textParams.size();i++)
     {
        if (textParams[i]->name == name)
        {
           textParams[i]->type = type;
           textParams[i]->value = value;
           //insertLoadedParam(param, type, value);
        }
     }
         //cerr << "<param: " << name << ", " << type << ":" << value << ">\n";
      }
      par = par->next;
      
   }


   modified = false;

   //updating all networks
   updateAllNetworks();

}



UINetwork *UIDocument::newNetwork(const string &_name, UINetwork::Type type)
{
   //cerr << "UIDocument::newNetwork\n";
   return new UINetwork(this, _name, type);
}

UINetwork *UIDocument::newNetwork(xmlNodePtr _net)
{
   //cerr << "UIDocument::newNetwork\n";
   return new UINetwork(this, _net);
}


UINetwork *UIDocument::addNetwork(string name, UINetwork::Type type)
{
   //cerr << "UIDocument::addNetwork (type = " << typeid(this).name() << ")" << endl;
   
   //UINetwork *newNet = new GUINetwork(this, name, iter);

  bool found = false;
  
  for (unsigned int i = 0; i < networks.size(); i++) {
    if (networks[i]->getName() == name) {
      found = true;
      break;
    }
  }
  
  if (found) {
    throw new GeneralException(string("Network already exist : ") + string(name), __FILE__, __LINE__);
  }

   UINetwork *newNet = newNetwork(name, type);
   for (unsigned int i=0;i<networks.size();i++)
   {
      networks[i]->newNetNotify("Subnet",name);
      newNet->newNetNotify("Subnet",networks[i]->getName());
   }
   networks.insert(networks.end(), newNet);
   modified = true;
   return newNet;
}

UINetwork *UIDocument::addNetwork(xmlNodePtr xmlNet)
{
   //cerr << "creating...\n";
   UINetwork *newNet = newNetwork(xmlNet);
   //cerr << "created\n";
   //cerr << "newNet = " << newNet << endl;
   //cerr << "network created in UIDocument::addNetwork\n";
   for (unsigned int i=0;i<networks.size();i++)
   {
      networks[i]->newNetNotify("Subnet",newNet->getName());
      newNet->newNetNotify("Subnet",networks[i]->getName());
   }
   //cerr << "newNet = " << newNet << endl;
   networks.insert(networks.end(), newNet);

   modified = true;
   return newNet;
}

//This function looks useless. Is it?
void UIDocument::removeNetwork(UINetwork *toRemove)
{
   vector<UINetwork *>::iterator i=networks.begin();
      while (i != networks.end())
      {
         if (*i == toRemove)
	 {
	    delete (*i);
            networks.erase(i);
	    break;
	 }
	 ++i;
      }

   setModified();
}

void UIDocument::error(char *err)
{
   cerr << err << endl;
}

void UIDocument::save()
{
   string fullname = path+docName;
   int size;

   int save_fd = open(fullname.c_str(), O_CREAT|O_WRONLY|O_TRUNC, 00755);
   if (save_fd==-1)
   {
      error("Error while saving file: cannot open");
      return;
   }
   fd_ostream outFile(save_fd);
   if (outFile.fail())
   {
      error("Error while saving file");
      return;
   }
   
   char *mem = saveToMemory(size);
   outFile << "#!/usr/bin/env batchflow" << endl;
   outFile.write(mem, size);
   if (outFile.fail())
   {
      free(mem);
      error("Error while saving file");
      return;
   }
   
   free(mem);
   resetModified();

}


char *UIDocument::saveToMemory(int &size)
{
   xmlDocPtr doc;
   doc = xmlNewDoc((xmlChar *)"1.0");
   doc->children = xmlNewDocNode(doc, NULL, (xmlChar *)"Document", NULL);

   //saving category if defined
   if (category!="") {
      xmlSetProp(doc->children, (xmlChar *)"category", (xmlChar *)category.c_str());
   }

   //saving comments if defined
   if (m_comments!="") {
     xmlSetProp(doc->children, (xmlChar *)"comments", (xmlChar *)m_comments.c_str());
   }


   for (unsigned int i=0;i<networks.size();i++)
   {
      networks[i]->saveXML(doc->children);
   }

   for (unsigned int i=0;i<textParams.size();i++)
   {
      xmlNodePtr tree;
      tree = xmlNewChild(doc->children, NULL, (xmlChar *)"Parameter", NULL);
      xmlSetProp(tree, (xmlChar *)"name", (xmlChar *)textParams[i]->name.c_str());
      xmlSetProp(tree, (xmlChar *)"type", (xmlChar *)textParams[i]->type.c_str());
      xmlSetProp(tree, (xmlChar *)"value", (xmlChar *)textParams[i]->value.c_str());
   }
   
   char *mem;
   xmlDocDumpFormatMemory(doc,(xmlChar **)&mem,&size, 1);

   xmlFreeDoc(doc);
   return mem;
}

string UIDocument::findExternal(const string &filename, char *searchPath, bool include_home, bool fullPathOutput)
{
   vector<string> pathlist = envList(searchPath, include_home);
   string fullname;
   for (unsigned int i=0;i<pathlist.size();i++)
   {
      if (findExternalRecursive(pathlist[i],"", filename,fullname, fullPathOutput))
	  return fullname;
   }
   return "";
}

bool UIDocument::findExternalRecursive(const string &basePath, const string &path, const string &filename, string &fullname, bool fullPathOutput)
{
   struct stat my_stat;
   string dirPath = basePath + "/" + path;
   DIR *my_directory = opendir (dirPath.c_str());

   if (!my_directory) return false;

   struct dirent *current_entry;

   for (current_entry = readdir(my_directory); 
	current_entry != NULL; current_entry = readdir(my_directory)) {
    
      string name = current_entry->d_name;
      string fullpath = basePath + "/" + path + string("/") + name;

      if (stat(fullpath.c_str(), &my_stat) < 0) {
	 //cerr<<"stat error"<<endl;
	 perror(fullpath.c_str());
	 continue;
      }
    
      if (S_ISDIR(my_stat.st_mode)) {
	 //it is a directory, let's doing it recursively
	 if (name != string("..") && name != string(".")) {
	    if (findExternalRecursive(basePath, path + "/" + name,filename,fullname, fullPathOutput))
	    {
	       closedir(my_directory);
	       return true;
	    }
	 }
      }
      else {
	 //it's a file, check if it's the right one
	 if (name == filename) {
	    if (fullPathOutput)
	       fullname = fullpath;
	    else
	       fullname = path + string("/") + name;
	    closedir(my_directory);
	    return true;
	 }
      }
   }

   closedir(my_directory);
   return false;
   
}

Network *UIDocument::buildExternal(const string &type, const string &_name, const ParameterSet &params)
{
   string fullname = findExternal(type + ".n");
   if (fullname == "")
      return NULL;
   UIDocument doc(fullname);
   
   //cout<<"loading : "<<fullpath<<endl;
   doc.load();
   
   UINetwork *net = doc.getNetworkNamed("MAIN");
   if (net)
      return net->build(_name, params);
   else
      throw new GeneralException("No MAIN network defined", __FILE__, __LINE__);
}


Network *UIDocument::build(const string &_name, const ParameterSet &params)
{
   Network *net = NULL;
   try {
      UINetwork *uinet = getNetworkNamed("MAIN");
      if (!uinet)
	 throw new GeneralException("No MAIN network defined", __FILE__, __LINE__);
      net = uinet->build(_name, params);
      net->verifyConnect();
      return net;
   } catch (BaseException *e)
   {
      e->freeze();
      if (net)
      {
	 net->cleanupNotify();
	 delete net;
      }
      throw e;
   } catch (...)
   {
      if (net)
      {
	 net->cleanupNotify();
	 delete net;
      }
      throw;
   }
}


void UIDocument::genCodeExternal(const string &type, ostream &out, int &id, set<string> &nodeList)
{
   string fullname = findExternal(type+".n");
   if (fullname == "")
      throw new GeneralException(string("External node not found: ") + type, __FILE__, __LINE__);
   UIDocument doc(fullname);
   doc.load();
   UINetwork *uinet = doc.getNetworkNamed("MAIN");
   if (!uinet)
      throw new GeneralException("No MAIN network defined", __FILE__, __LINE__);
   uinet->genCode(out, id, nodeList);
}

set<string> UIDocument::genCode(ostream &out, const string &functName, bool localIncludes)
{
   set<string> nodeList;
   out << "//This code has been generated automatically using codeflow\n";
   out << "//Note that automatic code generation is in a very experimental\n";
   out << "//  stage right now, use at your own risk\n";
   if (localIncludes)
   {
      out << "#include \"Network.h\"\n";
      out << "#include \"Iterator.h\"\n";
      out << "#include \"object_param.h\"\n\n\n";
   } else {
      out << "#include <Network.h>\n";
      out << "#include <Iterator.h>\n";
      out << "#include <object_param.h>\n\n\n";
   }
   int id=0;
   UINetwork *uinet = getNetworkNamed("MAIN");
   if (!uinet)
      throw new GeneralException("No MAIN network defined", __FILE__, __LINE__);   
   uinet->genCode(out, id, nodeList);
   out << "Network *" << functName << "(const string &_name, ParameterSet &params)" << endl;
   out << "{\n";
   out << "\tNetwork *net = genNet0(_name, params);\n";
   
   //Don't verify... in case we need other connections
   //out << "\tnet->verifyConnect();\n";
   
   out << "\treturn net;\n";
   out << "}\n";
   //cerr << "nodes used:\n";
   //for (set<string>::iterator it=nodeList.begin();it!=nodeList.end();it++)
   //   cerr << *it << endl;
   return nodeList;
}

//Run without a GUI
void UIDocument::run()
{
   Network *net = NULL;
   try {
      ParameterSet params;
      //cerr << "building net...\n";
      net = build("MAIN", params);
      if (net->getInputNode())
	 throw new GeneralException ("main network has input node", __FILE__, __LINE__);
      //cerr << "initializing...\n";
      net->initialize();
      //cerr << "running (UIDocument)...\n";
      
      for (int i = 0; ;i++) {
	 if (!net->hasOutput(i)) break;
	 cout << *net->getOutput(i,0);
      }
   } catch (BaseException *e) 
   {
      e->print();
   } catch (...) 
   {
      cerr << "unknown exception caught" << endl;
   }

   if (net)
   {
      net->cleanupNotify();
      delete net;
   }
}

void UIDocument::run(ParameterSet &p)
{
   Network *net=NULL;
   try {
      //cerr << "building net...\n";
      net = build("MAIN", p);
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
   } 
   catch (BaseException &e) {
      e.print();
   }
   catch (BaseException *e) 
   {
      e->print();
   }
   if (net)
   {
      net->cleanupNotify();
      delete net;
   }
}

void UIDocument::setFullPath(const string &fullpath)
{
   //cerr << "fullpath is: \"" << fullpath << "\"" << endl;
   int slashpos = fullpath.rfind("/");
   //cerr << "slashpos = " << slashpos << endl;
   path="";
   path.append(fullpath,0,slashpos+1);
   docName=fullpath;
   docName.erase(0,slashpos+1);
   //cerr << "path is: \"" << path << "\"" << endl;
   //cerr << "name is: \"" << name << "\"" << endl;
   untitled=false; 
}

void UIDocument::updateNetInfo(UINetwork *net) {

  //change our net information
  subnetInfo.updateNetInfo(net);

  //update "networks parameters that included this net as a node"
  for (int i = 0; i < networks.size(); i++) {  
    if (networks[i]) {
      networks[i]->updateAllSubnetParameters(net->getName(), subnetInfo.findNode(net->getName()));
    }
  }
}

void UIDocument::updateAllNetworks() {

  //update network information
  for (unsigned int i=0;i<networks.size();i++) {
    //subnetInfo.updateNetInfo(networks[i]);
    updateNetInfo(networks[i]);
  }
  
}


void UIDocument::updateAllSubnetTerminals(const string _nettype, const string _terminalname, 
					  UINetTerminal::NetTermType _terminaltype, bool _remove) 
{
   
   for (unsigned int i = 0; i < networks.size(); i++) 
   {
      if (networks[i])
         networks[i]->updateAllSubnetTerminals(_nettype, _terminalname, _terminaltype, _remove);
   }
}
