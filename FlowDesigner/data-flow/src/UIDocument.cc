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

#ifndef WIN32
#include <dlfcn.h>
#endif

#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <sstream>
#include <fstream>
#include <fcntl.h>
#include "stream_wrap.h"
#include "object_param.h"

//@implements UIClasses
using namespace std;

namespace FD {
	
	UIDocument::UIDocument(string _name)
	: modified(false)
	, editable(true)
	, docName(_name)
	, untitled(true)
	, destroyed(false)
	, m_connectionPort(DEFAULT_CONNECTION_PORT)
	, m_networkEventReceiver(this)
	{
		setFullPath(_name);
	}
	
	UIDocument::~UIDocument()
	{
		if (!destroyed)
		{
			//cerr << "destroying UIDocument " << name << endl;
			std::vector<UINetwork *>::iterator netIt = networks.begin();
			while(netIt != networks.end())
			{
				networks.erase(netIt);
				delete (*netIt);
				netIt = networks.begin();
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
			
			//Notify observers
			for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
			{
				(*iter)->notifyDestroyed(this);
			}
			
			
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
	
	
	void UIDocument::addParameterText(const string &name, const string &value, const string &type, const string &description)
	{
		ItemInfo *textInfo = new ItemInfo;
		textInfo->name = name;
		textInfo->value = value;
		textInfo->type = type;
		textInfo->description = description;
		textParams.insert(textParams.end(), textInfo);
		
		//Notify observers
		for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyParametersChanged(this,textInfo);
		}
		
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
			cerr << "load: error loading " << fullpath << "\n";
			addNetwork("MAIN", UINetwork::subnet);
			setModified(false);
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
					error("Error: this doesn't look like an FlowDesigner document");
					addNetwork("MAIN", UINetwork::subnet);
					setModified(false);
					return;
				}
			}
		} else if (ch!='<')
		{
			error("Error: this doesn't look like an FlowDesigner document");
			addNetwork("MAIN", UINetwork::subnet);
			setModified(false);
			return;
		}
		string xmlStr;
		docFile >> xmlStr;
		if (xmlStr != "?xml")
		{
			error("Error: this doesn't look like an FlowDesigner document");
			addNetwork("MAIN", UINetwork::subnet);
			setModified(false);
			return;
		}
		//docFile.putback(ch);
		string docStr="<?xml";
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
		//cerr<<"loading XML document from memory"<<endl;
		loadFromMemory(docStr.c_str(), docStr.size());
		//cerr<<"done!"<<endl;
		
		//subnetInfo.Print(cerr);
	}
	
	void UIDocument::loadFromMemory(const char *mem, int size)
	{
		xmlDocPtr doc = xmlParseMemory (const_cast<char *> (mem), size);
		if (!doc || !doc->children || !doc->children->name)
		{
			error("Error: corrupted XML in file");
			addNetwork("MAIN", UINetwork::subnet);
			setModified(false);
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
		
		//loading name if it exists
		xmlChar *name = xmlGetProp(root, (xmlChar *)"docName");
		if (name)
		{
			docName = string((char *)name);
			free (name);
		}
		
		//loading comments if they exists
		xmlChar *comments = xmlGetProp(root, (xmlChar *)"comments");
		if (comments)
		{
			m_comments = string((char *)comments);
			free (comments);
		}
		
		//loading port if it exists
		xmlChar *port = xmlGetProp(root, (xmlChar *)"connectionPort");
		if (port)
		{
			m_connectionPort = std::atoi((char *)port);
			free (comments);
		}
		
		//loading networks
		xmlNodePtr net = root->children;
		//cerr << "parsing...\n";
		while (net != NULL)
		{
			//Standard network found
			if (string((char*)net->name) == "Network")
			{
				addNetwork (net);
			}
			//File included (prototype)
			if (string((char*)net->name) == "IncludeNetwork")
			{
				
				cerr<<"Warning, included network is still a prototype, use at your own risk"<<endl;
				xmlChar *fname = xmlGetProp(net,(xmlChar *)"file");
				
				if (fname)
				{
					cerr<<"Including : "<<(char*) fname<<endl;
					try
					{
						//let's import the network
						importNetwork(string((char*) fname));
						
					}
					catch(BaseException *e)
					{
						e->print(cerr);
						delete e;
					}
					free(fname);
				}
			}
			
			
			
			net = net->next;
		}
		
		
		vector<ItemInfo *> tmp = getNetParams("MAIN");
		//cerr << "Got " << tmp.size() << " params in GUIDocument::createParamDialog\n";
		//textParams.resize(tmp.size());
		for (unsigned int i=0;i<tmp.size();i++)
		{
			ItemInfo *newParam = new ItemInfo;
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
		
		
		setModified(false);
		
		//updating all networks
		updateAllNetworks();
		
	}
	
	
	
	UINetwork *UIDocument::newNetwork(const string &_name, UINetwork::Type type)
	{
		//cerr << "UIDocument::newNetwork\n";
		UINetwork *newNet =  new UINetwork(this, _name, type);
		if (newNet)
		{	
			updateNetInfo(newNet);
			newNet->registerEvents(&m_networkEventReceiver);
		}
		return newNet;
	}
	
	UINetwork *UIDocument::newNetwork(xmlNodePtr _net)
	{
		//cerr << "UIDocument::newNetwork\n";
		UINetwork *newNet =  new UINetwork(this, _net);
		if (newNet)
		{	
			updateNetInfo(newNet);
			newNet->registerEvents(&m_networkEventReceiver);
		}
		return newNet;
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

		
		/*
		for (unsigned int i=0;i<networks.size();i++)
		{
			networks[i]->newNetNotify("Subnet",name);
			newNet->newNetNotify("Subnet",networks[i]->getName());
		}
		*/
		
		networks.insert(networks.end(), newNet);
		
		//Notify observers
		for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyNetworkAdded(this,newNet);
		}
		
		setModified(true);
		return newNet;
	}
	
	UINetwork *UIDocument::addNetwork(xmlNodePtr xmlNet)
	{
		
		//cerr << "creating...\n";
		UINetwork *newNet = newNetwork(xmlNet);
		
		//look for existing network before adding
		UINetwork *tmp_net = getNetworkNamed(newNet->getName());
		
		if (tmp_net != NULL) {
			string netName = newNet->getName();
			delete newNet;
			throw new GeneralException(string("Network (") + netName + string(") already exists"),__FILE__,__LINE__);
		}
		
		
		//cerr << "created\n";
		//cerr << "newNet = " << newNet << endl;
		//cerr << "network created in UIDocument::addNetwork\n";
		/*
		for (unsigned int i=0;i<networks.size();i++)
		{
			networks[i]->newNetNotify("Subnet",newNet->getName());
			newNet->newNetNotify("Subnet",networks[i]->getName());
		}
		*/ 
		 
		//cerr << "newNet = " << newNet << endl;
		networks.insert(networks.end(), newNet);
		
		//Notify observers
		for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyNetworkAdded(this,newNet);
		}
		
		setModified(true);
		return newNet;
	}
	
	//This function looks useless. Is it?
	void UIDocument::removeNetwork(UINetwork *toRemove, bool deleteNetwork)
	{
		vector<UINetwork *>::iterator i=networks.begin();
		while (i != networks.end())
		{
			if (*i == toRemove)
			{
				//Notify observers
				for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
				{
					(*iter)->notifyNetworkRemoved(this,*i);
				}
				
				//Watch out, must be removed from list before deleting
				networks.erase(i);
				
				if (deleteNetwork)
				{	
					delete (*i);
				}
				
				
				break;
			}
			++i;
		}
		
		setModified(true);
	}
	
	void UIDocument::error(const char *err)
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
		setModified(false);
		
	}
	
	
	char *UIDocument::saveToMemory(int &size)
	{
		xmlDocPtr doc;
		doc = xmlNewDoc((xmlChar *)"1.0");
		doc->children = xmlNewDocNode(doc, NULL, (xmlChar *)"Document", NULL);
		
		//saving the document name
		xmlSetProp(doc->children, (xmlChar *)"docName", (xmlChar *)docName.c_str());
		
		//saving category if defined
		if (category!="") {
			xmlSetProp(doc->children, (xmlChar *)"category", (xmlChar *)category.c_str());
		}
		
		//saving comments if defined
		if (m_comments!="") {
			xmlSetProp(doc->children, (xmlChar *)"comments", (xmlChar *)m_comments.c_str());
		}
		
		//saving the port
		char bufPort[6] = {0};
		sprintf(bufPort, "%d", m_connectionPort);
		xmlSetProp(doc->children, (xmlChar *)"connectionPort", (xmlChar *)bufPort);
		
		int incId = 1;
		for (unsigned int i=0;i<networks.size();i++)
		{
			networks[i]->saveXML(doc->children, incId);
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
	
	string UIDocument::findExternal(const string &filename, const char *searchPath, bool include_home, bool fullPathOutput)
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
		//cout<<"Building external ... : "<<type<<endl;
		string fullname = findExternal(type + ".n");
		if (fullname == "")
			return NULL;
		UIDocument doc(fullname);
		
		//cout<<"loading : "<<fullpath<<endl;
		doc.load();
		
		UINetwork *net = doc.getNetworkNamed("MAIN");
		if (net)
		{
			return net->build(_name, params);
		}
		else
		{
			throw new GeneralException("No MAIN network defined", __FILE__, __LINE__);
		}
	}
	
	
	Network *UIDocument::build(const string &_name, const ParameterSet &params)
	{
		
		//cerr<<"Building network  :"<<_name<<endl;
		
		Network *net = NULL;
		try {
			UINetwork *uinet = getNetworkNamed("MAIN");
			if (!uinet)
				throw new GeneralException("No MAIN network defined", __FILE__, __LINE__);
			
			//copy params
			ParameterSet myParams = params;
			
			//adding document params
			for (size_t i = 0; i < textParams.size(); i++)
			{
				if (!myParams.exist(textParams[i]->name)) {
					//create object
					ObjectRef param_value = ObjectParam::stringParam(textParams[i]->type,textParams[i]->value,myParams);
					
					//adding default param value if not specified into arguments
					myParams.add(textParams[i]->name,param_value);
				}
			}
			
			
			net = uinet->build(_name, myParams);
			
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
		out << "using namespace std;\n";
		out << "using namespace FD;\n\n\n";
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
		unsigned int slashpos = fullpath.rfind("/");
		//cerr << "slashpos = " << slashpos << endl;
		if (slashpos != string::npos)
		{
			path="";
			path.append(fullpath,0,slashpos+1);
			docName=fullpath;
			docName.erase(0,slashpos+1);
		}
		//cerr << "path is: \"" << path << "\"" << endl;
		//cerr << "name is: \"" << name << "\"" << endl;
		untitled=false;
		
		//Notify observers
		for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyPathChanged(this,path);
			(*iter)->notifyNameChanged(this,docName);
		}
	}
	
	void UIDocument::updateNetInfo(UINetwork *net) 
	{
		
		//change our net information
		subnetInfo.updateNetInfo(net);
		
		//update "networks parameters that included this net as a node"
		for (size_t i = 0; i < networks.size(); i++)
		{
			if (networks[i] && networks[i] != net) 
			{
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
	
	void UIDocument::exportNetwork(const std::string &networkName, const std::string &fileName) {
		
		UINetwork *net = getNetworkNamed(networkName);
		
		if (net) {
			
			int save_fd = open(fileName.c_str(), O_CREAT|O_WRONLY|O_TRUNC, 00755);
			if (save_fd == -1) {
				error("UIDocument::exportNetwork : Error while saving file: cannot open");
				return;
			}
			fd_ostream outFile(save_fd);
			if (outFile.fail()) {
				error("UIDocument::exportNetwork : Error while saving file");
				return;
			}
			
			xmlDocPtr doc;
			doc = xmlNewDoc((xmlChar *)"1.0");
			doc->children = xmlNewDocNode(doc, NULL, (xmlChar *)"Document", NULL);
			
			
			//Export network
			int startId = 1;
			net->saveXML(doc->children, startId);
			
			char *mem = NULL;
			int size = 0;
			
			//Dump to memory
			xmlDocDumpFormatMemory(doc,(xmlChar **)&mem,&size, 1);
			xmlFreeDoc(doc);
			
			//Write to file
			outFile.write(mem, size);
			if (outFile.fail()) {
				free(mem);
				error("UIDocument::exportNetwork : Error while saving file");
				return;
			}
			
			//Free memory
			free(mem);
		}
		else {
			//TODO throw an exception ?
			throw new GeneralException(string("Network does not exist :") + networkName,__FILE__,__LINE__);
		}
	}
	
	
	
	void UIDocument::importNetwork(const std::string &fileName) {
		
		//TODO Merge stuff with load to avoid duplication of the code.
		//The difference is that we do not reset document data before loading
		//and we do not create a "MAIN" network.
		
		string docStr;
		
		ifstream inputFile(fileName.c_str());
		
		if (!inputFile.fail()) {
			
			char ch;
			inputFile >> ch;
			if (ch=='#')
			{
				//let's read the leading characters (script like format)
				while (ch != '<')
				{
					inputFile >> ch;
					if (inputFile.fail())
					{
						error("Error: this doesn't look like an FlowDesigner document");
						setModified(false);
						return;
					}
				}
			} else if (ch!='<')
			{
				error("Error: this doesn't look like an FlowDesigner document");
				setModified(false);
				return;
			}
			string xmlStr;
			inputFile >> xmlStr;
			if (xmlStr != "?xml")
			{
				error("Error: this doesn't look like an FlowDesigner document");
				setModified(false);
				return;
			}
			
			//put back xml starting string
			docStr = "<?xml";
			
			//read all data from file
			while(1) {
				char buff[1025];
				inputFile.read(buff, 1024);
				buff[1024]=0;
				if (inputFile.fail()) {
					docStr.append(buff, inputFile.gcount());
					break;
				}
				docStr.append(buff, 1024);
			}
			
			try {
				//loading document
				xmlDocPtr doc = xmlParseMemory (const_cast<char *> (docStr.c_str()), docStr.size());
				
				if (!doc || !doc->children || !doc->children->name) {
					throw new GeneralException(string("Corrupted XML in file ") + fileName,__FILE__,__LINE__);
				}
				
				xmlNodePtr net = doc->children->children;
				
				if (net) {
					
					//loading all networks
					while (net != NULL) {
						
						//Standard network
						if (string((char*)net->name) == "Network") {
							addNetwork (net);
						}
						
						//File included (prototype)
						if (string((char*)net->name) == "IncludeNetwork") {
							
							cerr<<"Warning, included network is still a prototype, use at your own risk"<<endl;
							xmlChar *fname = xmlGetProp(net,(xmlChar *)"file");
							
							if (fname) {
								cerr<<"(Recursive) Including : "<<(char*) fname<<endl;
								try {
									//let's import the network
									importNetwork(string((char*) fname));
									
								}
								catch(BaseException *e) {
									e->print(cerr);
									delete e;
								}
								free(fname);
							}//Valid filename
						}//IncludeNetwork
						net = net->next;
					}//while net
				}
				
				//free XML data
				xmlFreeDoc(doc);
				
				//the network is modified
				setModified(true);
			}
			catch (BaseException *e) {
				throw e->add(new GeneralException(string("Unable to import from file ") + fileName,__FILE__,__LINE__));
			}
		}
		else {
			throw new GeneralException(string("File does not exist : ") + fileName,__FILE__,__LINE__);
		}
	}
	
	/**Sets the 'modified' flag*/
	void UIDocument::setModified(bool flag)
	{
		// Only notify if modified is changed
		if((flag && !modified) || (!flag && modified)) {
			modified = flag;
			//Notify observers
			for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
			{
				(*iter)->notifyChanged(this);
			}
		}
	}
	
	/**Set the category of the document in the node menu */
	void UIDocument::setCategory(const std::string &cat)
	{
		category = cat;
		
		//Notify observers
		for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyCategoryChanged(this,category);
		}
	}
	
	/**Get the category of the document in the node menu */
	const std::string& UIDocument::getCategory()
	{
		return category;
	}
	
	/**Set comments for the document */
	void UIDocument::setComments(const std::string &comments)
	{
		m_comments = comments;
		
		//Notify observers
		for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyCommentsChanged(this,m_comments);
		}
	}
	
	/**Get comments from the document */
	const std::string& UIDocument::getComments()
	{
		return m_comments;
	}
	
	std::vector<UINetwork *> UIDocument::get_networks()
	{
		return networks;
	}
	
	std::vector<ItemInfo *> UIDocument::get_textParams()
	{
		return textParams;
	}
	
	UINodeRepository& UIDocument::getRepository()
	{
		return subnetInfo;
	}
	
	
	void UIDocument::registerEvents(UIDocumentObserverIF *observer)
	{
		if (find(m_observers.begin(),m_observers.end(),observer) == m_observers.end())
		{
			m_observers.push_back(observer);
		}
	}
	
	
	void UIDocument::unregisterEvents(UIDocumentObserverIF *observer)
	{
		for (std::list<UIDocumentObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			if (*iter == observer)
			{
				m_observers.erase(iter);
				break;
			}
		}
	}
	
	void UIDocument::handleSubnetNameChanged(UINetwork *net, const std::string &oldName, const std::string &newName)
	{
		//First, update local repository with new info.
		updateNetInfo(net);
		
		//Remove old info
		subnetInfo.removeNode(oldName);
		
		//Type of inserted subnet (as nodes) must be changed to new name
		for(size_t i = 0; i < networks.size(); i++)
		{
			vector<UINode*> allNodes = networks[i]->getNodes();
			for (size_t j = 0; j < allNodes.size(); j++)
			{
				if (allNodes[j]->getType() == oldName)
				{
					allNodes[j]->setType(newName);
				}
			}
		}
		
	}
	
	
	
}
