// Copyright (C) 2001 Jean-Marc Valin

#include "path.h"
#include <tree.h>
#include <parser.h>
#include "UIDocument.h"
#include "UINetwork.h"
//#include "UINodeMenu.h"
#include "Node.h"
#include "Network.h"
#include "ParameterSet.h"
#include <sys/stat.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>

//@implements UIClasses

UIDocument::UIDocument(string _name)
   : modified(false)
   , docName(_name)
   , untitled(true)
{
}

UIDocument::~UIDocument()
{
   //cerr << "destroying UIDocument " << name << endl;
   for (unsigned int i=0;i<networks.size();i++)
      delete networks[i];

   for (unsigned int i=0;i<textParams.size();i++)
      delete textParams[i];

   for (unsigned int i=0;i<docInputs.size();i++)
      delete docInputs[i];

   for (unsigned int i=0;i<docOutputs.size();i++)
      delete docOutputs[i];

   for (unsigned int i=0;i<docParams.size();i++)
      delete docParams[i];
}


UINetwork *UIDocument::getNetworkNamed(const string &n)
{
   //cerr << "UIDocument::getNetworkNamed\n";
   //cerr << n << endl;
   //cerr << "size = " << networks.size() << endl;
   for (unsigned int i=0;i<networks.size();i++)
   {
      //cerr << "ptr = " << &(networks[i]) << endl;
      //cerr << networks[i]->getName() << " == " << n << endl;
      if (networks[i]->getName() == n)
         return networks[i];
   }
   //cerr << "net not found in doc\n";
   return NULL;
}

vector<ItemInfo *> UIDocument::getNetInputs(const string &netName)
{
   UINetwork *net = getNetworkNamed(netName);
   vector <ItemInfo *> inputs;
   if (net)
   {
      vector<string> tmp = net->getTerminals(UINetTerminal::INPUT);
      for (unsigned int i = 0; i < tmp.size(); i++)
      {
            ItemInfo *newInfo = new ItemInfo;
            newInfo->name = tmp[i];
            inputs.insert(inputs.end(), newInfo);
      }
     
   } else if (subnetInfo.findNode(netName))
      return subnetInfo.findNode(netName)->inputs;

/*else if (preloadInfo.find(netName) != preloadInfo.end()) {
      inputs = preloadInfo[netName]->inputs;
   } else 
      if (UINodeRepository::Find(netName))
      return UINodeRepository::Find(netName)->inputs;*/

   return inputs;
}

vector<ItemInfo *> UIDocument::getNetOutputs(const string &netName)
{
   UINetwork *net = getNetworkNamed(netName);
   vector <ItemInfo *> outputs;
   if (net)
   {
      vector<string> tmp = net->getTerminals(UINetTerminal::OUTPUT);
      for (unsigned int i = 0; i < tmp.size(); i++)
      {
            ItemInfo *newInfo = new ItemInfo;
            newInfo->name = tmp[i];
            outputs.insert(outputs.end(), newInfo);
      }
       
   } else if (subnetInfo.findNode(netName))
      return subnetInfo.findNode(netName)->outputs;

    /*else if (preloadInfo.find(netName) != preloadInfo.end()) {
      outputs = preloadInfo[netName]->outputs;
   } else 
      if (UINodeRepository::Find(netName))
      return UINodeRepository::Find(netName)->outputs;*/

   return outputs;
}

vector<ItemInfo *> UIDocument::getNetParams(const string &netName)
{
   UINetwork *net = getNetworkNamed(netName);
   vector <ItemInfo *> params;
   if (net)
   {
      net->insertNetParams(params);
   } else if (subnetInfo.findNode(netName))
      return subnetInfo.findNode(netName)->params;

   /*else if (preloadInfo.find(netName) != preloadInfo.end()) 
   {
      params = preloadInfo[netName]->params;
   } else 
      if (UINodeRepository::Find(netName))
      return UINodeRepository::Find(netName)->params;*/

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
/*
void UIDocument::loadNetInfo(xmlNodePtr net, map<string, NodeInfo *> &infoMap, string netName)
{
   if (netName == "")
      netName = string((char *)xmlGetProp(net, (CHAR *)"name"));
   
   CHAR *category = xmlGetProp(net, (CHAR *)"category");
   
   
   if (infoMap.find(netName) != infoMap.end())
   {
      cerr << "error: net " << netName << " already existed\n";
      return;
   }
   //cerr << "new subnet info with name: " << netName << "\n";
   NodeInfo *info = new NodeInfo;
   infoMap[netName] = info;
   
   if (category)
      info->category = string((char *)category);
   
   //cerr << "scan all nodes\n";
   xmlNodePtr node = net->childs;
   while (node != NULL)
   {
      if (string((char*)node->name) == "Node")
      {
	 
	 
	 xmlNodePtr par = node->childs;
	 //cerr << "par = " << par << endl;
	 while (par)
	 {
	    if (string((char*)par->name) == "Parameter")
	    {
	       string paramName = string ((char *) xmlGetProp(par, (CHAR *)"value"));
	       string type = string ((char *) xmlGetProp(par, (CHAR *)"type"));
	       if (type == "subnet_param")
	       {
		  bool alreadyPresent = false;
		  for (unsigned int j=0;j<info->params.size();j++)
		     if (info->params[j]->name == paramName)
			alreadyPresent=true;
		  if (!alreadyPresent)
		  {
		     ItemInfo *newInfo = new ItemInfo;
		     newInfo->name = paramName;
		     //FIXME: This always sets the type to subnet_info, which is incorrect
		     newInfo->type = type;
		     info->params.insert (info->params.end(), newInfo);
		  }
	       }          
	    }
	    par = par->next;
	    
	 }
	 
	 
	 
	 //check for params
      }
      node = node->next;
   }
   
   //scan all net inputs/outputs
   node = net->childs;
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
*/
void UIDocument::printOn(ostream &out) const
{
   out << "<UIDocument" << endl;
   out << "<name " << docName << " >" << endl;
   out << ">" << endl;
}
/*
void UIDocument::loadAllSubnetInfo(xmlNodePtr net)
{
   //cerr << "UIDocument::loadNetInfo\n";
   //cerr << "this = " << this << endl;
   while (net != NULL)
   {
      if (string((char*)net->name) == "Network")
      {
     //cerr << "scanning a net\n";

     loadNetInfo(net, preloadInfo);
     
      }
      net = net->next;
   }
}
*/

void UIDocument::load()
{
   string fullpath=path+docName;
   xmlDocPtr doc = xmlParseFile(fullpath.c_str());
   if (!doc || !doc->root || !doc->root->name)
   {
      cerr << "load: error loading " << fullpath << "\n";
      xmlFreeDoc (doc);
      addNetwork("MAIN", UINetwork::subnet);
      resetModified();
      return;
   }
   xmlNodePtr root=doc->root;
   loadXML(root);
   xmlFreeDoc(doc);

}

void UIDocument::loadFromMemory(char *mem, int size)
{
   xmlDocPtr doc = xmlParseMemory (mem, size);
   xmlNodePtr root=doc->root;
   if (!doc || !doc->root || !doc->root->name)
   {
      cerr << "load: error: cannot parse memory\n";
      xmlFreeDoc (doc);
      return;
   }
   loadXML(root);
   xmlFreeDoc(doc);

}

void UIDocument::loadXML(xmlNodePtr root)
{
   //loadAllSubnetInfo(root->childs);
   subnetInfo.loadAllSubnetInfo(root->childs);

   xmlNodePtr net = root->childs;
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
   xmlNodePtr par = root->childs;
   //cerr << "par = " << par << endl;
   
   while (par)
   {
      if (string((char*)par->name) == "Parameter")
      {
         string name = string ((char *) xmlGetProp(par, (CHAR *)"name"));
         string type = string ((char *) xmlGetProp(par, (CHAR *)"type"));
         string value = string ((char *) xmlGetProp(par, (CHAR *)"value"));
         
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
   xmlDocPtr doc;
   doc = xmlNewDoc((CHAR *)"1.0");
   doc->root = xmlNewDocNode(doc, NULL, (CHAR *)"Document", NULL);
   for (unsigned int i=0;i<networks.size();i++)
   {
      networks[i]->saveXML(doc->root);
   }

   for (unsigned int i=0;i<textParams.size();i++)
   {
      xmlNodePtr tree;
      tree = xmlNewChild(doc->root, NULL, (CHAR *)"Parameter", NULL);
      xmlSetProp(tree, (CHAR *)"name", (CHAR *)textParams[i]->name.c_str());
      xmlSetProp(tree, (CHAR *)"type", (CHAR *)textParams[i]->type.c_str());
      xmlSetProp(tree, (CHAR *)"value", (CHAR *)textParams[i]->value.c_str());
   }

   string fullname = path+docName;
   if (xmlSaveFile(fullname.c_str(), doc)==-1)
   {
      error("Error while saving file");
   } else {
      resetModified();
   }

   xmlFreeDoc(doc);
}

void UIDocument::export2net()
{
   string netName = path+docName+"et";
   ofstream out(netName.c_str());;
   for (unsigned int i=0;i<networks.size();i++)
   {
      networks[i]->export2net(out);
   }
   
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

/*
 * Hack to get the available nodes.
 * Returns a vector of strings, with each string having the following format:
 * 
 * category***type
 * -> category is the category the node falls under (with subcategories
 * separated with :'s
 * -> type is the unique description of the node type
 */
/*
vector<string> UIDocument::getAvailableNodes()
{
	vector<string> allNodes;
	string nextItem;
	// first look at the externalDocInfo 
	
	allNodes = UINodeRepository::Available();

	// now look at the preloadInfo
	map<string, NodeInfo *>::iterator iter = preloadInfo.begin();
	
	while (iter != preloadInfo.end()) {
		nextItem = string((*iter).second->category) + "***" + 
				   string((*iter).first);
		allNodes.insert(allNodes.end(), nextItem);
		iter++;
	}
	return allNodes;
								
}
											

*/
