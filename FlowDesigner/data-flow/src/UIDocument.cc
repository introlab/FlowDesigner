#include <tree.h>
#include <parser.h>
#include "UIDocument.h"
#include "UINetwork.h"
//#include "UINodeMenu.h"
#include "Node.h"
#include "Network.h"
#include "ParameterSet.h"
#include <sys/stat.h>


map<string, SubnetInfo *> UIDocument::externalDocInfo;


UIDocument::UIDocument(string _name)
   : docName(_name)
   , untitled(true)
   , modified(false)
{
   //cerr << "created doc with name " << docName << endl;
   //gnome_mdi_child_set_name (GNOME_MDI_CHILD(this), (gchar *)name.c_str());
   //create();
}

UIDocument::~UIDocument()
{
   //cerr << "destroying UIDocument " << name << endl;
   for (int i=0;i<networks.size();i++)
      delete networks[i];
   //child.name = "destroyed document";
}


UINetwork *UIDocument::getNetworkNamed(const string &n)
{
   //cerr << "UIDocument::getNetworkNamed\n";
   //cerr << n << endl;
   //cerr << "size = " << networks.size() << endl;
   for (int i=0;i<networks.size();i++)
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
      for (int i = 0; i < tmp.size(); i++)
      {
            ItemInfo *newInfo = new ItemInfo;
            newInfo->name = tmp[i];
            inputs.insert(inputs.end(), newInfo);
      }
     
   } else if (preloadInfo.find(netName) != preloadInfo.end()) {
      inputs = preloadInfo[netName]->inputs;
   } else if (externalDocInfo.find(netName) != externalDocInfo.end()) {
      inputs = externalDocInfo[netName]->inputs;
   }
   return inputs;
}

vector<ItemInfo *> UIDocument::getNetOutputs(const string &netName)
{
   UINetwork *net = getNetworkNamed(netName);
   vector <ItemInfo *> outputs;
   if (net)
   {
      vector<string> tmp = net->getTerminals(UINetTerminal::OUTPUT);
      for (int i = 0; i < tmp.size(); i++)
      {
            ItemInfo *newInfo = new ItemInfo;
            newInfo->name = tmp[i];
            outputs.insert(outputs.end(), newInfo);
      }
       
   } else if (preloadInfo.find(netName) != preloadInfo.end()) {
      outputs = preloadInfo[netName]->outputs;
   } else if (externalDocInfo.find(netName) != externalDocInfo.end()) {
      outputs = externalDocInfo[netName]->outputs;
   }
   return outputs;
}

vector<ItemInfo *> UIDocument::getNetParams(const string &netName)
{
   //cerr << "UIDocument::getNetParams for document: " << docName << "\n";
   UINetwork *net = getNetworkNamed(netName);
   //cerr << "got the network\n";
   vector <ItemInfo *> params;
   if (net)
   {
      //cerr << "net is already loaded...\n";
      /*vector<string> paramNames;
      paramNames.resize(preloadInfo[netName]->params.size());
    
      for (int i = 0; i < preloadInfo[netName]->params.size(); i++)
          paramNames[i] = preloadInfo[netName]->params[i]->name;*/
      
      net->insertNetParams(params);
   } else if (preloadInfo.find(netName) != preloadInfo.end()) {
      //cerr << "net is not loaded...\n";
      params = preloadInfo[netName]->params;
   } else if (externalDocInfo.find(netName) != externalDocInfo.end()) {
      //cerr << "net is not loaded...\n";
      params = externalDocInfo[netName]->params;
   }
   return params;
}

string UIDocument::getDescription(const string &type)
{
	string descr;
	if (externalDocInfo.find(type) != externalDocInfo.end()) 
		descr = externalDocInfo[type]->description;
	else
		descr = "Description not available.";
		
	return descr;
}


void UIDocument::addParameterText(string name, string value, string type)
{
    DocParameterDataText *textInfo = new DocParameterDataText;
    textInfo->name = name;
    textInfo->value = value;
    textInfo->type = type;
    textParams.insert(textParams.end(), textInfo);
}
    
void UIDocument::loadNetInfo(xmlNodePtr net, map<string, SubnetInfo *> &infoMap, string netName)
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
   SubnetInfo *info = new SubnetInfo;
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
          for (int j=0;j<info->params.size();j++)
             if (info->params[j]->name == paramName)
            alreadyPresent=true;
          if (!alreadyPresent)
              {
                  ItemInfo *newInfo = new ItemInfo;
                  newInfo->name = paramName;
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

void UIDocument::load()
{
   string fullpath=path+docName;
   xmlDocPtr doc = xmlParseFile(fullpath.c_str());
   xmlNodePtr root=doc->root;
   if (!doc || !doc->root || !doc->root->name)
   {
      cerr << "load: error loading " << fullpath << "\n";
      xmlFreeDoc (doc);
      return;
   }
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
   loadAllSubnetInfo(root->childs);

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
   
   for (int i=0;i<tmp.size();i++)
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
         
         for (int i=0;i<textParams.size();i++)
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


/***************************************************************************************************
 *                                                                                                 *
 * The code starting from this point is a big mess, it would need to be rewritten almost completly *
 *                                                                                                 *
 ***************************************************************************************************/

static vector<string> envList(char *envName)
{
   vector<string> list;
   char *strPath = getenv(envName);
   if (!strPath)
      return list;
   string path = strPath; 
   int start = 0;
   int pos = 0;
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

void UIDocument::loadNodeDefInfo(const string &path, const string &name)
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
     SubnetInfo *info = new SubnetInfo;
     info->category = string((char *)xmlGetProp(node, (CHAR *)"category"));
     nodeName = string((char *)xmlGetProp(node, (CHAR *)"name"));
     externalDocInfo[nodeName] = info;
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
      
      node = node->next;
   }
   xmlFreeDoc(doc);
   
}

void UIDocument::loadExtDocInfo(const string &path, const string &name)
{
   string fullname = path + "/" + name;
   xmlDocPtr doc = xmlParseFile(fullname.c_str());
   
   if (!doc || !doc->root || !doc->root->name)
   {
      cerr << "ExtDoc: error loading " << fullname << "\n";
      xmlFreeDoc (doc);
      return;
   }
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
        string basename = string(name.begin(), name.end()-2);
        loadNetInfo(net, externalDocInfo, basename);
     }
      }
      net = net->next;
   }
   xmlFreeDoc(doc);

}

#include <sys/types.h>

#include <dirent.h>

//
//Fixed to load recursively all toolboxes starting with the directories included
//In the VFLOW_PATH environment variable. 
//Dominic Letourneau Feb. 20 2001
//

void UIDocument::loadAllInfo()
{


  vector<string> dirs = envList("VFLOW_PATH");


  for (int i=0;i<dirs.size();i++) {
    loadAllInfoRecursive(dirs[i]);
  }


}


void UIDocument::loadAllInfoRecursive(const string &path) {

  struct stat my_stat;
  DIR *my_directory = opendir (path.c_str());

  if (!my_directory) return;

  struct dirent *current_entry;

  for (current_entry = readdir(my_directory); 
       current_entry != NULL; current_entry = readdir(my_directory)) {
    
    string name = current_entry->d_name;
    string fullpath = path + string("/") + name;

    if (stat(fullpath.c_str(), &my_stat) < 0) {
      cerr<<"stat error"<<endl;
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
      if (strstr(current_entry->d_name, ".n") && !strstr(current_entry->d_name, ".nn")){
	cout<<"Loading network : "<<fullpath<<endl;
        loadExtDocInfo(path, name);
      }
      
      //loading toolbox
      if (strstr(current_entry->d_name, ".def")) {
	cout<<"Loading toolbox : "<<fullpath<<endl;
        loadNodeDefInfo(path, name);
      }

    }
  }

  closedir(my_directory);
}


#include <dlfcn.h>

void UIDocument::scanDL()
{
   try {
      vector<string> dirs=envList("VFLOW_PATH");
      if (dirs.size() == 0)
      {
     cerr << "Did you forget to set the VFLOW_PATH environment to the directory where the libraries are installed?\n";
      }
      for (int i = 0; i<dirs.size();i++)
      {
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
        if (!dlopen(fullname.c_str(), RTLD_LAZY))
           cerr << "Toolbox load error: " << dlerror() << endl;
     }
     
     closedir(my_directory);
      }
      //UIDocument::loadExtDocInfo(files);
   } catch (BaseException *e)
   {
      e->print();
      delete e;
   } catch (...)
   {
      cerr << "unknown exception caught\n";
   }
   
}


UINetwork *UIDocument::newNetwork(UIDocument *_doc, const string &_name, UINetwork::Type type)
{
   //cerr << "UIDocument::newNetwork\n";
   return new UINetwork(_doc, _name, type);
}

UINetwork *UIDocument::newNetwork(UIDocument *_doc, xmlNodePtr _net)
{
   //cerr << "UIDocument::newNetwork\n";
   return new UINetwork(_doc, _net);
}


UINetwork *UIDocument::addNetwork(string name, UINetwork::Type type)
{
   //cerr << "UIDocument::addNetwork (type = " << typeid(this).name() << ")" << endl;
   
   //UINetwork *newNet = new GUINetwork(this, name, iter);
   UINetwork *newNet = newNetwork(this, name, type);
   for (int i=0;i<networks.size();i++)
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
   UINetwork *newNet = newNetwork(this, xmlNet);
   //cerr << "created\n";
   //cerr << "newNet = " << newNet << endl;
   //cerr << "network created in UIDocument::addNetwork\n";
   for (int i=0;i<networks.size();i++)
   {
      networks[i]->newNetNotify("Subnet",newNet->getName());
      newNet->newNetNotify("Subnet",networks[i]->getName());
   }
   //cerr << "newNet = " << newNet << endl;
   networks.insert(networks.end(), newNet);

   modified = true;
   return newNet;
}

void UIDocument::removeNetwork(UINetwork *toRemove)
{
   //ANSI C++ fix
   vector<UINetwork *>::iterator i=networks.begin();
      while (i != networks.end())
      {
         if (*i == toRemove)
	 {
            networks.erase(i);
	    break;
	 }
	 ++i;
      }
   /*for (int i = 0; i < networks.size(); i++)
       if (networks[i] == toRemove)
           networks.erase(&networks[i]);
   */      
   setModified();
}

void UIDocument::save()
{
   xmlDocPtr doc;
   doc = xmlNewDoc((CHAR *)"1.0");
   doc->root = xmlNewDocNode(doc, NULL, (CHAR *)"Document", NULL);
   for (int i=0;i<networks.size();i++)
   {
      networks[i]->saveXML(doc->root);
   }

   for (int i=0;i<textParams.size();i++)
   {
      xmlNodePtr tree;
      tree = xmlNewChild(doc->root, NULL, (CHAR *)"Parameter", NULL);
      xmlSetProp(tree, (CHAR *)"name", (CHAR *)textParams[i]->name.c_str());
      xmlSetProp(tree, (CHAR *)"type", (CHAR *)textParams[i]->type.c_str());
      xmlSetProp(tree, (CHAR *)"value", (CHAR *)textParams[i]->value.c_str());
   }

   string fullname = path+docName;
   xmlSaveFile(fullname.c_str(), doc);

   xmlFreeDoc(doc);
}

void UIDocument::export2net()
{
   string netName = path+docName+"et";
   ofstream out(netName.c_str());;
   for (int i=0;i<networks.size();i++)
   {
      networks[i]->export2net(out);
   }
   
}

#include <sys/stat.h>
#include <unistd.h>

Network *UIDocument::buildExternal(const string &type, const string &_name, const ParameterSet &params)
{
   string filename = "";
   vector<string> pathlist = envList("VFLOW_PATH");
   for (int i=0;i<pathlist.size();i++)
   {
      string fullname = pathlist[i]+"/"+type+".n";
      struct stat buf;
      //cerr << "trying " << fullname << endl;
      if (stat(fullname.c_str() , &buf) == 0)
      {
     //cerr << "found!\n";
     filename = fullname;
     break;
      }
   }
   if (filename == "")
   {
      cerr << "error loading: " << type << " not found\n";
      return NULL;
   }
   UIDocument doc(filename);
   doc.load();
   Network *net = doc.getNetworkNamed("MAIN")->build(_name, params);
   //cerr << "net = " << net << endl;
   return net;
   //cerr << "UIDocument::build\n";
   //return getNetworkNamed("MAIN")->build(_name, params);
}

Network *UIDocument::build(const string &_name, const ParameterSet &params)
{
   //cerr << "UIDocument::build\n";
   return getNetworkNamed("MAIN")->build(_name, params);
}

//Run without a GUI
void UIDocument::run()
{
   try {
      ParameterSet params;
      //cerr << "building net...\n";
      Network *net = build("MAIN", params);
      if (net->getInputNode())
     throw new GeneralException ("main network has input node", __FILE__, __LINE__);
      //cerr << "initializing...\n";
      net->initialize();
      //cerr << "running (UIDocument)...\n";
      
      for (int i = 0; ;i++) {
     if (!net->hasOutput(i)) break;
     cout << *net->getOutput(i,0);
      }
   }
   catch (BaseException &e) {
      e.print();
   }
   catch (BaseException *e) {
      e->print();
   }
   
}

void UIDocument::run(ParameterSet &p)
{
   try {
      //cerr << "building net...\n";
      Network *net = build("MAIN", p);
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
   catch (BaseException *e) {
      e->print();
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
vector<string> UIDocument::getAvailableNodes()
{												 
	vector<string> allNodes;
	string nextItem;
	// first look at the externalDocInfo 
	map<string, SubnetInfo *>::iterator iter = externalDocInfo.begin();
	
	while (iter != externalDocInfo.end()) {
		nextItem = string((*iter).second->category) + "***" + 
				   string((*iter).first);
		allNodes.insert(allNodes.end(), nextItem);
		iter++;											 
	}
				
	// now look at the preloadInfo
	iter = preloadInfo.begin();
	
	while (iter != preloadInfo.end()) {
		nextItem = string((*iter).second->category) + "***" + 
				   string((*iter).first);
		allNodes.insert(allNodes.end(), nextItem);
		iter++;
	}
													 
	return allNodes;
								
}
													 
											
