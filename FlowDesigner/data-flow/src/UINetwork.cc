// Copyright (C) 2001 Jean-Marc Valin

#include "UINetwork.h"
#include "UIDocument.h"
#include "UINode.h"
#include "UITerminal.h"
#include <libxml/tree.h>
#include "UILink.h"
#include "UIProbeLink.h"
#include "Node.h"
#include "UINetTerminal.h"
#include "UINote.h"

#include "Network.h"
#include "Iterator.h"
#include "ParameterSet.h"

#ifndef WIN32
#include "ThreadedIterator.h"
#endif

//UINetwork *currentNetwork;

//@implements UIClasses

using namespace std;

namespace FD {
	
	UINetwork::UINetwork(UIDocument *_doc, string _name, Type _type)
	: doc(_doc), name(_name), type (_type), buildRecurs(false)
	{		
		//(DL 04/08/2004)
		//Adding version to newly created network, could be improved
		createNote(string("Automatic-note"), string("Created with FlowDesigner ") + string(FLOWDESIGNER_VERSION),0,0,false);
		
	}
	
	UINetwork::UINetwork(UIDocument *_doc, xmlNodePtr net)
	: doc(_doc), buildRecurs(false)
	{
		load(net);
	}
	
	void UINetwork::load (xmlNodePtr net)
	{
		char *netName = (char *)xmlGetProp(net, (xmlChar *)"name");
		if (!netName)
			throw new GeneralException("No network name", __FILE__, __LINE__);
		name = string(netName);
		free(netName);
		
		//(DL) 12/12/2003 Loading network description if available
		char *netDescription = (char *)xmlGetProp(net, (xmlChar *)"description");
		if (netDescription) {
			m_description = string(netDescription);
			free(netDescription);
		}
		
		char *netType = (char *)xmlGetProp(net, (xmlChar *)"type");
		
		if (!netType)
			type=subnet;
		else {
			if (netType == string("subnet"))
				type=subnet;
			else if (netType == string("iterator"))
				type=iterator;
			else if (netType == string("threaded"))
				type=threaded;
		}
		free(netType);
		//name(_name)
		//create();
		//cerr << "parsing nodes\n";
		xmlNodePtr node = net->children;
		while (node != NULL)
		{
			if (string((char*)node->name) == "Node")
			{
				//This looks strange (like a leak), but the network stores the pointer
				loadNode (node);
			}
			node = node->next;
		}
		//cerr << "parsing links\n";
		node = net->children;
		
		while (node != NULL)
		{
			if (string((char*)node->name) == "Link" || string((char*)node->name) == "ProbeLink")
			{
				
				char *str_id = (char *)xmlGetProp(node, (xmlChar *)"id");
				
				char *str_fromnode = (char *)xmlGetProp(node, (xmlChar *)"from");
				char *str_out = (char *)xmlGetProp(node, (xmlChar *)"output");
				char *str_tonode = (char *)xmlGetProp(node, (xmlChar *)"to");
				char *str_in = (char *)xmlGetProp(node, (xmlChar *)"input");
				int id = 0;
				if(str_id) {
					id = atoi(str_id);
				}
				string fromnode = string(str_fromnode);
				string out = string(str_out);
				string tonode = string(str_tonode);
				string in = string(str_in);
				
				/*cerr << fromnode << ":" << out << " -> " << tonode << ":" << in << endl;
				 cerr << getNodeNamed(fromnode)->getOutputNamed(out) << " "
				 << getNodeNamed(tonode)->getInputNamed(in) << endl;*/
				// Free memory
				if(str_id) // Older version don't have id properties
					free(str_id);
				free(str_fromnode);
				free(str_out);
				free(str_tonode);
				free(str_in);
				
				char *points=NULL;
				if (node->children)
					points = (char *)node->children->content;
				if (getNodeNamed(fromnode) && getNodeNamed(tonode))
				{
					if (!getNodeNamed(tonode)->getInputNamed(in))
						getNodeNamed(tonode)->addTerminal(string(in), UINetTerminal::INPUT);
					if (!getNodeNamed(fromnode)->getOutputNamed(out))
						getNodeNamed(fromnode)->addTerminal(string(out), UINetTerminal::OUTPUT);
					
					createLink(getNodeNamed(fromnode)->getOutputNamed(out),
							getNodeNamed(tonode)->getInputNamed(in), points, id);
				} else {
					cerr << "Invalid link from " << fromnode << ":" << out << " to "
					<< tonode << ":" << in << endl;
				}
			}
			node = node->next;
		}
		node = net->children;
		while (node != NULL)
		{
			if (string((char*)node->name) == "NetInput")
			{
				char *str_termName = (char *)xmlGetProp(node, (xmlChar *)"name");
				char *str_termTerm = (char *)xmlGetProp(node, (xmlChar *)"terminal");
				char *str_termNode = (char *)xmlGetProp(node, (xmlChar *)"node");
				//(DL) 15/12/2003 reading obj type & description
				char *str_termObjType = (char*)xmlGetProp(node, (xmlChar *) "object_type");
				char *str_termDescription = (char*)xmlGetProp(node, (xmlChar *) "description");
				
				string termName = string(str_termName);
				string termTerm = string(str_termTerm);
				string termNode = string(str_termNode);
				
				string termType = "any"; //default to "any"
				if (str_termObjType) {
					termType = string(str_termObjType);
					free(str_termObjType);
				}
				
				string termDesc = "No description available"; //default to no description available
				if (str_termDescription) {
					termDesc = string(str_termDescription);
					free(str_termDescription);
				}
				
				//Free XML strings
				free(str_termName); free(str_termTerm); free(str_termNode);
				
				if (getNodeNamed(termNode))
				{
					//update terminal (all info)
					if (!getNodeNamed(termNode)->getInputNamed(termTerm))
						getNodeNamed(termNode)->addTerminal(termTerm, UINetTerminal::INPUT, termType, termDesc);
					
					//update Net terminal (all info) for the subnet
					createNetTerminal(getNodeNamed(termNode)->getInputNamed(termTerm),
								   UINetTerminal::INPUT, termName, termType, termDesc);
					
					
				} else {
					cerr << "Invalid netTerminal at " << termNode << ":" << termTerm << endl;
				}
			} else if (string((char*)node->name) == "NetOutput")
			{
				char *str_termName = (char *)xmlGetProp(node, (xmlChar *)"name");
				char *str_termTerm = (char *)xmlGetProp(node, (xmlChar *)"terminal");
				char *str_termNode = (char *)xmlGetProp(node, (xmlChar *)"node");
				//(DL) 15/12/2003 reading obj type & description
				char *str_termObjType = (char*)xmlGetProp(node, (xmlChar *) "object_type");
				char *str_termDescription = (char*)xmlGetProp(node, (xmlChar *) "description");
				
				
				string termName = string(str_termName);
				string termTerm = string(str_termTerm);
				string termNode = string(str_termNode);
				
				string termType = "any"; //default to "any"
				if (str_termObjType) {
					termType = string(str_termObjType);
					free(str_termObjType);
				}
				
				string termDesc = "No description available"; //default to no description available
				if (str_termDescription) {
					termDesc = string(str_termDescription);
					free(str_termDescription);
				}
				
				
				
				free(str_termName); free(str_termTerm); free(str_termNode);
				
				if (getNodeNamed(termNode))
				{
					
					//update terminal (all info)
					if (!getNodeNamed(termNode)->getOutputNamed(termTerm)) {
						getNodeNamed(termNode)->addTerminal(termTerm, UINetTerminal::OUTPUT, termType, termDesc);
					}
					
					//update Net terminal (all info) for the subnet
					createNetTerminal(getNodeNamed(termNode)->getOutputNamed(termTerm),
								   UINetTerminal::OUTPUT, termName, termType, termDesc);
				} else {
					cerr << "Invalid netTerminal at " << termNode << ":" << termTerm << endl;
				}
			} else if (string((char*)node->name) == "NetCondition")
			{
				char *str_termName = (char *)xmlGetProp(node, (xmlChar *)"name");
				char *str_termTerm = (char *)xmlGetProp(node, (xmlChar *)"terminal");
				char *str_termNode = (char *)xmlGetProp(node, (xmlChar *)"node");
				string termName = string(str_termName);
				string termTerm = string(str_termTerm);
				string termNode = string(str_termNode);
				free(str_termName); free(str_termTerm); free(str_termNode);
				
				if (getNodeNamed(termNode))
				{
					if (!getNodeNamed(termNode)->getOutputNamed(termTerm))
						getNodeNamed(termNode)->addTerminal(termTerm, UINetTerminal::OUTPUT);
					createNetTerminal(getNodeNamed(termNode)->getOutputNamed(termTerm),
								   UINetTerminal::CONDITION, termName);
				} else {
					cerr << "Invalid netTerminal at " << termNode << ":" << termTerm << endl;
				}
			}
			else if (string((char*)node->name) == "Note")
			{
				//(DL 04/08/2004) Added Note (Post-It like) in the network description
				char *str_noteLabel = (char *) xmlGetProp(node,(xmlChar*)"label");
				char *str_noteText = (char *) xmlGetProp(node,(xmlChar*)"text");
				char *str_noteX = (char*) xmlGetProp(node,(xmlChar*)"x");
				char *str_noteY = (char*) xmlGetProp(node,(xmlChar*)"y");
				char *str_noteVisible = (char*) xmlGetProp(node,(xmlChar*)"visible");
				
				//convert that into strings
				
				
				string noteLabel = "No Label";
				if (str_noteLabel)
					noteLabel = string(str_noteLabel);
				
				string noteText(str_noteText);
				string noteX(str_noteX);
				string noteY(str_noteY);
				string noteVisible(str_noteVisible);
				
				//Must absolutely free memory to avoid leaks
				
				if (str_noteLabel)
					free(str_noteLabel);
				
				free(str_noteText);
				free(str_noteX);
				free(str_noteY);
				free(str_noteVisible);
				
				stringstream noteXStream(noteX);
				stringstream noteYStream(noteY);
				stringstream noteVisibleStream(noteVisible);
				
				double x,y;
				noteXStream>>x;
				noteYStream>>y;
				bool visible;
				noteVisibleStream>>visible;
				
				
				
				//Creating / Adding new note
				m_notes.push_back(createNote(noteLabel,noteText,x,y,visible));
			}
			node = node->next;
		}
		//cerr << "done...\n";
		
		
	}
	
	
	
	UINetwork::~UINetwork()
	{
		
		//Links are deleted through the nodes destructor
		for (unsigned int i=0;i<nodes.size();i++)
			delete nodes[i];
		
		for (unsigned int i=0;i<terminals.size();i++)
			delete terminals[i];
		
		for (unsigned int i=0;i<m_notes.size();i++)
			delete m_notes[i];
		
		//send signal
		for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyDestroyed(this);
		}
		
		//Make sure we are removed from the document
		if (doc)
		{
			doc->removeNetwork(this,false);
		}
		
		
	}
		
	UINode *UINetwork::getNodeNamed(string n)
	{
		for (unsigned int i=0;i<nodes.size();i++)
			if (nodes[i]->getName() == n)
				return nodes[i];
		return NULL;
	}
	
	bool UINetwork::addNode(UINode *node)
	{
		
		if (find(nodes.begin(),nodes.end(),node) == nodes.end())
		{	
			nodes.insert(nodes.end(), node);
			doc->setModified(true);
			
			//send signal
			for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
			{
				(*iter)->notifyNodeAdded(this,node);
			}
			
			return true;
		}
		
		return false;
	}
	
	
	bool UINetwork::removeNode(UINode *node, bool deleteNode)
	{
		vector<UINode*>::iterator i = find(nodes.begin(),nodes.end(),node);
		if (i != nodes.end())
		{
/*			
			//Make sure we remove all inputs and outputs
			std::vector<UITerminal *> nodeInputs = node->getInputs();
			std::vector <UITerminal *> nodeOutputs = node->getOutputs();
			
			for (size_t index  =0; index < nodeInputs.size(); index++)
			{
				node->removeTerminal(nodeInputs[index],true);
			}
			
			for (size_t index  =0; index < nodeOutputs.size(); index++)
			{
				node->removeTerminal(nodeOutputs[index],true);
			}
*/			
			//remove from list
			nodes.erase(i);
			
			
			//delete node if necessary
			//This must be done before, othewise observers will receive the notifyNodeRemoved before terminals and links
			if (deleteNode)
			{
				delete node;
			}
			
			//notify observers
			for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
			{
				(*iter)->notifyNodeRemoved(this,node);
			}
			
	
			
			if (doc)
				doc->setModified(true);
			
			return true;
		}	
		
		return false;
	}
	
	bool UINetwork::addLink(UILink *link)
	{
		if (find(links.begin(), links.end(), link) == links.end())
		{	
			links.insert(links.end(), link);
			doc->setModified(true);
			
			//send signal
			for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
			{
				(*iter)->notifyLinkAdded(this,link);
			}
		
			return true;
		}
		
		return false;
	}
	
	bool UINetwork::removeLink(UILink *link, bool deleteLink)
	{
		//Fix to be ANSI C++
		vector<UILink *>::iterator i=find(links.begin(),links.end(),link);
		
		if (i != links.end())
		{	

			links.erase(i);
			
			if (deleteLink)
				delete link;
			
			//send signal
			for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
			{
				(*iter)->notifyLinkRemoved(this,link);
			}
		
			doc->setModified(true);
		}
		
		return false;
	}
	
	void UINetwork::saveXML(xmlNode *root, int &incId)
	{
		xmlNodePtr tree;
		//tree = xmlNewChild(root, NULL, (xmlChar *)"",  (xmlChar*)"toto");
		tree = xmlNewChild(root, NULL, (xmlChar *)"Network", NULL);
		switch (type)
		{
			case subnet:
				xmlSetProp(tree, (xmlChar *)"type", (xmlChar *)"subnet");
				break;
			case iterator:
				xmlSetProp(tree, (xmlChar *)"type", (xmlChar *)"iterator");
				break;
			case threaded:
				xmlSetProp(tree, (xmlChar *)"type", (xmlChar *)"threaded");
				break;
		}
		xmlSetProp(tree, (xmlChar *)"name", (xmlChar *)name.c_str());
		
		//(DL) 12/12/2003 Saving network description if available
		if (m_description != "") {
			xmlSetProp(tree, (xmlChar *)"description", (xmlChar *)m_description.c_str());
		}
		
		/*if (isIterator && conditionNode)
		 xmlSetProp(tree, (xmlChar *)"condition", (xmlChar *)conditionNode->getName().c_str());*/
		
		for (unsigned int i=0;i<nodes.size();i++)
		{
			nodes[i]->saveXML(tree);
		}
		for (unsigned int i=0;i<links.size();i++)
		{
			links[i]->saveXML(tree, incId++);
		}
		for (unsigned int i=0;i<terminals.size();i++)
		{
			terminals[i]->saveXML(tree);
		}
		//(DL 04/08/2004) Saving notes
		for (unsigned int i=0;i<m_notes.size(); i++)
		{
			m_notes[i]->saveXML(tree);
		}
		
	}
	
	void UINetwork::setModified()
	{
		doc->setModified(true);
		//Not a good idea at all...
		//interfaceChangeNotify();
	}
	
	vector<string> UINetwork::getTerminals(UINetTerminal::NetTermType termType)
	{
		vector<string> term;
		for (unsigned int i=0;i<terminals.size();i++)
		{
			UINetTerminal::NetTermType type = terminals[i]->getType();
			if (type == termType)
				term.insert(term.end(), terminals[i]->getName());
		}
		return term;
	}

	/*
	void UINetwork::newNetNotify(const string &cat, const string &type)
	{
		//if (type != name)
		//   popup->addType(cat,type);
	}
	*/
	 
	void UINetwork::insertNetParams(vector<ItemInfo *> &params)
	{
		for (unsigned int i=0;i<nodes.size();i++)
		{
			nodes[i]->insertNetParams(params);
		}
		
		if (type == iterator)
		{
			ItemInfo *newInfo = new ItemInfo;
			newInfo->name = "DOWHILE";
			newInfo->type = "bool";
			params.insert(params.end(), newInfo);
		}
		// params.insert(params.end(), "DOWHILE");
		if (type == threaded)
		{
			ItemInfo *newInfo = new ItemInfo;
			newInfo->name = "RATE_PER_SECOND";
			newInfo->type = "int";
			params.insert(params.end(), newInfo);
		}
		// params.insert(params.end(), "RATE_PER_SECOND");
	}
	
	UINode *UINetwork::createNode(string _name, string _type, double _x, double _y)
	{
		//The node will add itself in the curent network
		return new UINode(this,_name, _type, _x, _y);
	}
	
	UINode *UINetwork::loadNode(xmlNodePtr def)
	{
		//The node will add itself in the current network
		return new UINode(this, def);
	}
	
	/*UITerminal *UINetwork::newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y)
	 {
	 return new UITerminal (_name, _node, _isInput, _x, _y);
	 }*/
	
	UILink *UINetwork::createLink (UITerminal *_from, UITerminal *_to, const char *str, int _id)
	{
		if (_from && _to && _to->getConnections().size() == 0)
		{	
			//The link will add itself in the current network
			return new UIProbeLink (_from, _to, str, _id);
		}
		
		return NULL;
	}
	
	UINote *UINetwork::createNote(const std::string &label, const std::string &text, double x, double y, bool visible) 
	{
		//The note will add itself in the current network
		return new UINote(this,label,text,x,y,visible);
	}
	
	
	UINetTerminal *UINetwork::createNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const string &_name, const string &_objType, const string &_description)
	{
		if (_terminal)
		{
			//The terminal will add itself int he current network
			return new UINetTerminal (_terminal, _type, _name, _objType, _description);
		}
		
		return NULL;
	}
	
	
	
	Network *UINetwork::build(const string &netName, const ParameterSet &params)
	{
		//cerr << "UINetwork::build\n";
		//cerr << "name = " << name << endl;
		if (buildRecurs)
			throw new GeneralException("Detected circular subnet inclusion, breaking infinite loop", __FILE__, __LINE__);
		
		Network *net;
		switch (type)
		{
			case iterator:
				net = new Iterator(netName, params);
				break;
			case subnet:
				net = new Network(netName, params);
				break;
#ifndef WIN32
			case threaded:
				net = new ThreadedIterator(netName, params);
				break;
#endif
			default:
				throw new GeneralException("Subnet of unknown type", __FILE__, __LINE__);
				break;
		}
		
		
		try
		{
			
			for (unsigned int i=0;i<nodes.size();i++)
			{
				//cerr << "building node " << nodes[i]->getName() << endl;
				
				buildRecurs=true;
				try {
					Node *aNode = nodes[i]->build(params);
					net->addNode(*aNode);
				} catch (...) {
					buildRecurs=false;
					throw;
				}
				buildRecurs=false;
				
			}
		} catch (BaseException *e)
		{
			throw e->add (new GeneralException(string("Exception caught while building network ") + name, __FILE__, __LINE__));
		}
		
		try {
			//cerr << "nodes built\n";
			
			//insert links
			for (unsigned int i=0;i<links.size();i++)
			{
				//cerr << "linking...\n";
				//This try/catch is a workaround (when using -fomit-frame-pointer) for bug #212990
				try {
					links[i]->build(net);
				} catch (BaseException *e)
				{
					/*cerr << "caught in UINetwork::build\n";*/
					throw e->add(new GeneralException ("Exception caught while building link in network " + name,
													   __FILE__, __LINE__));
				}
			}
			
			//cerr << "links built\n";
			
			bool inputExist=false;
			for (unsigned int i=0;i<terminals.size();i++)
			{
				UINetTerminal::NetTermType type = terminals[i]->getType();
				if (type == UINetTerminal::INPUT)
					inputExist=true;
			}
			
			if (inputExist)
			{
				ParameterSet empty;
				Node *node=NULL;
				_NodeFactory *factory = NULL;
				factory = Node::getFactoryNamed("Collector");
				node = factory->Create("ALL_NETWORK_INPUTS", empty);
				net->addNode(*node);
				net->setInputNode(node);
			}
			
			{
				ParameterSet empty;
				Node *node=NULL;
				_NodeFactory *factory = NULL;
				factory = Node::getFactoryNamed("Collector");
				node = factory->Create("ALL_NETWORK_OUTPUTS", empty);
				net->addNode(*node);
				net->setSinkNode(node);
			}
			
			if (type == iterator)
			{
				ParameterSet empty;
				Node *node=NULL;
				_NodeFactory *factory = NULL;
				factory = Node::getFactoryNamed("Collector");
				node = factory->Create("NETWORK_CONDITION", empty);
				net->addNode(*node);
				dynamic_cast<Iterator *>(net)->setConditionNode(node);
			}
			
			//cerr << "collector built\n";
			
			bool found_output=false;
			bool found_condition=false;
			for (unsigned int i=0;i<terminals.size();i++)
			{
				UINetTerminal::NetTermType type = terminals[i]->getType();
				if (type == UINetTerminal::INPUT)
				{
					net->connect(terminals[i]->getTerminal()->getNode()->getName(), terminals[i]->getTerminal()->getName(), "ALL_NETWORK_INPUTS", terminals[i]->getName());
				}
				if (type == UINetTerminal::OUTPUT)
				{
					net->connect("ALL_NETWORK_OUTPUTS", terminals[i]->getName(), terminals[i]->getTerminal()->getNode()->getName(), terminals[i]->getTerminal()->getName());
					found_output=true;
				}
				if (type == UINetTerminal::CONDITION)
				{
					net->connect("NETWORK_CONDITION", "OUTPUT", terminals[i]->getTerminal()->getNode()->getName(), terminals[i]->getTerminal()->getName());
					found_condition=true;
				}
				
			}
			
			if (!found_output)
				throw new GeneralException("No output defined for network", __FILE__,__LINE__);
			if (type!=subnet && !found_condition)
				throw new GeneralException("No condition defined for iterator", __FILE__,__LINE__);
			//insert netTerminals
		} catch (BaseException *e)
		{
			e->freeze();
			net->cleanupNotify();
			delete net;
			throw e;
		} catch (...)
		{
			net->cleanupNotify();
			delete net;
			throw;
		}
		return net;
	}
	
	void UINetwork::genCode(ostream &out, int &id, set<string> &nodeList)
	{
		int bakID=id;
		id++;
		vector<int> ids;
		
		try {
			for (unsigned int i=0;i<nodes.size();i++)
			{
				ids.push_back(id);
				nodes[i]->genCode(out, id, nodeList);
			}
		} catch (BaseException *e)
		{
			throw e->add(new GeneralException(string("Exception caught while building network ") + name,
											  __FILE__, __LINE__));
		}
		
		out << "static Network *genNet" << bakID << "(const string &netName, const ParameterSet &params)\n";
		out << "{\n";
		
		switch (type)
		{
			case iterator:
				out << "   Network *net = new Iterator(netName, params);\n";
				break;
			case subnet:
				out << "   Network *net = new Network(netName, params);\n";
				break;
			case threaded:
				out << "   Network *net = new ThreadedIterator(netName, params);\n";
				break;
		}
		
		
		out << "\n   Node *aNode;\n";
		for (unsigned int i=0;i<ids.size();i++)
		{
			out << "   aNode = genNode" << ids[i] << "(params);\n";
			out << "   net->addNode(*aNode);\n\n";
		}
		
		
		for (unsigned int i=0;i<links.size();i++)
		{
			links[i]->genCode(out);
		}
		
		
		
		bool inputExist=false;
		for (unsigned int i=0;i<terminals.size();i++)
		{
			UINetTerminal::NetTermType type = terminals[i]->getType();
			if (type == UINetTerminal::INPUT)
				inputExist=true;
		}
		
		if (inputExist)
		{
			out << "   {\n";
			out << "      ParameterSet empty;\n";
			out << "      Node *node=NULL;\n";
			out << "      _NodeFactory *factory = NULL;\n";
			out << "      factory = Node::getFactoryNamed(\"Collector\");\n";
			out << "      node = factory->Create(\"ALL_NETWORK_INPUTS\", empty);\n";
			out << "      net->addNode(*node);\n";
			out << "      net->setInputNode(node);\n";
			out << "   }\n";
		}
		
		{
			out << "   {\n";
			out << "      ParameterSet empty;\n";
			out << "      Node *node=NULL;\n";
			out << "      _NodeFactory *factory = NULL;\n";
			out << "      factory = Node::getFactoryNamed(\"Collector\");\n";
			out << "      node = factory->Create(\"ALL_NETWORK_OUTPUTS\", empty);\n";
			out << "      net->addNode(*node);\n";
			out << "      net->setSinkNode(node);\n";
			out << "   }\n";
		}
		
		if (type == iterator)
		{
			out << "   {\n";
			out << "      ParameterSet empty;\n";
			out << "      Node *node=NULL;\n";
			out << "      _NodeFactory *factory = NULL;\n";
			out << "      factory = Node::getFactoryNamed(\"Collector\");\n";
			out << "      node = factory->Create(\"NETWORK_CONDITION\", empty);\n";
			out << "      net->addNode(*node);\n";
			out << "      dynamic_cast<Iterator *>(net)->setConditionNode(node);\n";
			out << "   }\n";
		}
		
		
		bool found_output=false;
		bool found_condition=false;
		for (unsigned int i=0;i<terminals.size();i++)
		{
			UINetTerminal::NetTermType type = terminals[i]->getType();
			if (type == UINetTerminal::INPUT)
			{
				out << "   net->connect(\"" << terminals[i]->getTerminal()->getNode()->getName() << "\", \""
				<< terminals[i]->getTerminal()->getName() << "\", \"ALL_NETWORK_INPUTS\", \""
				<< terminals[i]->getName() << "\");\n";
			}
			if (type == UINetTerminal::OUTPUT)
			{
				out << "   net->connect(\"ALL_NETWORK_OUTPUTS\", \"" << terminals[i]->getName() << "\", \""
				<< terminals[i]->getTerminal()->getNode()->getName() << "\", \"" <<
				terminals[i]->getTerminal()->getName() << "\");\n";
				found_output=true;
			}
			if (type == UINetTerminal::CONDITION)
			{
				out << "   net->connect(\"NETWORK_CONDITION\", \"OUTPUT\", \""
				<< terminals[i]->getTerminal()->getNode()->getName() << "\", \""
				<< terminals[i]->getTerminal()->getName() << "\");\n";
				found_condition=true;
			}
			
		}
		
		if (!found_output)
		{
			throw new GeneralException("UINetwork::genCode: Network has no output", __FILE__, __LINE__);
		}
		
		if (type!=subnet && !found_condition)
		{
			throw new GeneralException("UINetwork::genCode: No condition defined for iterator", __FILE__, __LINE__);
		}
		
		
		out << "   return net;\n";
		
		out << "}\n\n";
	}
	
	bool UINetwork::addNetTerminal(UINetTerminal *term)
	{
		
		if (find(terminals.begin(),terminals.end(),term) == terminals.end())
		{	
		
			terminals.insert(terminals.end(), term);
			
			//send signal
			for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
			{
				(*iter)->notifyNetTerminalAdded(this,term);
			}
			
			//Document is modified
			if (doc)
			{
				doc->setModified(true);
			}
			
			return true;
		}
		
		return false;
	}
	
	bool UINetwork::removeNetTerminal(UINetTerminal *term, bool deleteNetTerminal)
	{
		vector<UINetTerminal *>::iterator i=find(terminals.begin(),terminals.end(),term);
		if (i != terminals.end())
		{	
			//remove terminal
			terminals.erase(i);
			
			//Delete terminal if required
			if (deleteNetTerminal)
				delete term;
			
			
			//send signal
			for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
			{
				(*iter)->notifyNetTerminalRemoved(this,term);
			}
			
			//Document modified
			doc->setModified(true);
			
			//Interface changed
			interfaceChangeNotify();
			
			return true;
		}
		
		return false;
	}
	
	void UINetwork::interfaceChangeNotify()
	{
		//TODO remove this function
		//doc->updateNetInfo(this);
	}
	
	void UINetwork::rename(string newName)
	{
		
		if (doc->getNetworkNamed(newName)) {
			throw new GeneralException(string("Network name already exist : ") + newName, __FILE__,__LINE__);
		}

		string oldName = name;
		name = newName;

		
		//send signal
		for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyNameChanged(this,oldName,newName);
		}
		doc->setModified(true);
		
	}
	
	
	void UINetwork::updateAllSubnetTerminals(const string _nettype, const string _terminalname,
											 UINetTerminal::NetTermType _terminaltype, bool _remove) {
		
		
		
		for (unsigned int i = 0; i < nodes.size(); i++) {
			
			if (nodes[i]->getType() == _nettype) {
				if (_remove) {
					// Removes a terminal to a node
					nodes[i]->removeTerminal(_terminalname,_terminaltype);
				}
				else {
					// Adds a terminal to a node
					switch(_terminaltype) {
						case UINetTerminal::INPUT:
							if (!nodes[i]->getInputNamed(_terminalname))
								nodes[i]->addTerminal(_terminalname,_terminaltype);
							break;
						case UINetTerminal::OUTPUT:
							if (!nodes[i]->getOutputNamed(_terminalname))
								nodes[i]->addTerminal(_terminalname,_terminaltype);
							break;
						case UINetTerminal::CONDITION:
							// do nothing
							break;
						default:
							throw new GeneralException("Invalid terminal type",__FILE__,__LINE__);
							break;
					}
				}
			}
		}
		
	}
	
	bool UINetwork::haveLink(UILink* link)
	{
		for (size_t i = 0; i < links.size(); i++)
		{
			if (link == links[i])
			{
				return true;
			}
		}
		return false;
	}
	
	
	void UINetwork::updateAllSubnetParameters(const string _nettype, NodeInfo * _info) {
		
		
		//cerr<<"UINetwork::updateAllSubnetParameters"<<endl;
		for (unsigned int i = 0; i < nodes.size(); i++) {
			
			if (nodes[i]->getType() == _nettype) {
				
				//cerr<<"found a node in the subnet if type "<<_nettype<<endl;
				if (_info) {
					nodes[i]->updateNetParams(_info->params);
				}
				
				//our interface may have changed.
				interfaceChangeNotify();
			}
		}//for all nodes
		
		
		
	}
	
	bool UINetwork::addNote(UINote *note) 
	{
		if (note) 
		{
			if (find(m_notes.begin(),m_notes.end(),note) == m_notes.end())
			{	
				//insert note
				m_notes.push_back(note);
				doc->setModified(true);
				
				//send signal
				for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
				{
					(*iter)->notifyNoteAdded(this,note);
				}
				
				return true;
			}	
		}
		
		return false;
	}
	
	
	bool UINetwork::removeNote(UINote *note, bool deleteNote)
	{
		
		if (note)
		{
			vector<UINote*>::iterator i = find(m_notes.begin(),m_notes.end(), note);
			if ( i != m_notes.end())
			{	
				//remove note
				m_notes.erase(i);
				
				if (deleteNote)
					delete note;
				
				//send signal
				for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
				{
					(*iter)->notifyNoteRemoved(this,note);
				}
				


				doc->setModified(true);
				
				return true;
			}
		}
	
	return false;
	}
	
	std::string UINetwork::getName() const
	{
		return name;
	}
	
	std::string UINetwork::getDescription() const
	{
		return m_description;
	}
	
	void UINetwork::setDescription(const std::string & description)
	{
		m_description = description;
		doc->setModified(true);
		
		//send signal
		for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			(*iter)->notifyDescriptionChanged(this,m_description);
		}
		
	}
	
	UINetwork::Type UINetwork::getType() const
	{
		return type;
	}
	
	UIDocument* UINetwork::getDocument()
	{
		return doc;
	}
	
	bool UINetwork::isIter()
	{
		return type==iterator;
	}
	
	std::vector<UINode *> UINetwork::getNodes()
	{
		return nodes;
	}
	
	std::vector<UILink *> UINetwork::getLinks()
	{
		return links;
	}
	
	std::vector<UINetTerminal *> UINetwork::getTerminals()
	{
		return terminals;
	}
	
	///Direct access to the note vector
	std::vector<UINote*> UINetwork::getNotes()
	{
		return m_notes;
	}
	
	
	void UINetwork::registerEvents(UINetworkObserverIF *observer)
	{
		if (find(m_observers.begin(),m_observers.end(),observer) == m_observers.end())
		{
			m_observers.push_back(observer);
		}
	}
	
	void UINetwork::unregisterEvents(UINetworkObserverIF *observer)
	{
		for (std::list<UINetworkObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			if (*iter == observer)
			{
				m_observers.erase(iter);
				break;
			}
		}
	}
	
	bool UINetwork::haveNetTerminal(UINetTerminal *netTerminal)
	{
		if (find(terminals.begin(),terminals.end(),netTerminal) != terminals.end())
		{
			return true;
		}	
	
		return false;
	}
	
}//namespace FD
