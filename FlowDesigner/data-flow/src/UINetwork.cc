// Copyright (C) 2001 Jean-Marc Valin

#include "UINetwork.h"
#include "UIDocument.h"
#include "UINode.h"
#include "UITerminal.h"
#include <tree.h>
#include "UILink.h"
#include "Node.h"
#include "UINetTerminal.h"

#include "Network.h"
#include "Iterator.h"
#include "ParameterSet.h"
#include "ThreadedIterator.h"
//UINetwork *currentNetwork;

//@implements UIClasses


UINetwork::UINetwork(UIDocument *_doc, string _name, Type _type)
   //: canvas(doc->getCanvas())
   : doc(_doc)
   , name(_name)
   , type (_type)
   , destroyed(false)
   , buildRecurs(false)
   //, conditionNode(NULL)
{
   
   //create();
}

UINetwork::UINetwork(UIDocument *_doc, xmlNodePtr net, bool init)
   : doc(_doc)
   , destroyed(false)
   , buildRecurs(false)
   //, conditionNode(NULL)
{
   if (init)
     load(net);
}

void UINetwork::load (xmlNodePtr net)
{
   char *netName = (char *)xmlGetProp(net, (CHAR *)"name");
   if (!netName)
      throw new GeneralException("No network name", __FILE__, __LINE__);
   name = string(netName);
   free(netName);
   char *netType = (char *)xmlGetProp(net, (CHAR *)"type");
   
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
   xmlNodePtr node = net->childs;
   while (node != NULL)
   {
      if (string((char*)node->name) == "Node")
      {
	 //This looks strange (like a leak), but the network stores the pointer
         UINode *theNewNode = loadNode (node);
      }
      node = node->next;
   }
   //cerr << "parsing links\n";
   node = net->childs;
   while (node != NULL)
   {
      if (string((char*)node->name) == "Link")
      {
	 char *str_fromnode = (char *)xmlGetProp(node, (CHAR *)"from");
	 char *str_out = (char *)xmlGetProp(node, (CHAR *)"output");
	 char *str_tonode = (char *)xmlGetProp(node, (CHAR *)"to");
	 char *str_in = (char *)xmlGetProp(node, (CHAR *)"input");
         string fromnode = string(str_fromnode);
         string out = string(str_out);
         string tonode = string(str_tonode);
         string in = string(str_in);
	 
         /*cerr << fromnode << ":" << out << " -> " << tonode << ":" << in << endl;
         cerr << getNodeNamed(fromnode)->getOutputNamed(out) << " "
         << getNodeNamed(tonode)->getInputNamed(in) << endl;*/
	 free(str_fromnode); free(str_out); free(str_tonode); free(str_in);
	 char *points=NULL;
	 if (node->childs)
	    points = (char *)node->childs->content;
	 if (getNodeNamed(fromnode) && getNodeNamed(tonode) && getNodeNamed(fromnode)->getOutputNamed(out) && getNodeNamed(tonode)->getInputNamed(in))
	 {
	    newLink(getNodeNamed(fromnode)->getOutputNamed(out),
	        getNodeNamed(tonode)->getInputNamed(in), points);
	 } else {
	    cerr << "Invalid link from " << fromnode << ":" << out << " to "
		 << tonode << ":" << in << endl;
	 }
      }
      node = node->next;
   }
   node = net->childs;
   while (node != NULL)
   {
      if (string((char*)node->name) == "NetInput")
      {
	 char *str_termName = (char *)xmlGetProp(node, (CHAR *)"name");
	 char *str_termTerm = (char *)xmlGetProp(node, (CHAR *)"terminal");
	 char *str_termNode = (char *)xmlGetProp(node, (CHAR *)"node");
	 string termName = string(str_termName);
	 string termTerm = string(str_termTerm);
	 string termNode = string(str_termNode);
	 free(str_termName); free(str_termTerm); free(str_termNode);

	 if (getNodeNamed(termNode) && getNodeNamed(termNode)->getInputNamed(termTerm))
	 {
	    newNetTerminal(getNodeNamed(termNode)->getInputNamed(termTerm),
			   UINetTerminal::INPUT, termName);
	 } else {
	    cerr << "Invalid netTerminal at " << termNode << ":" << termTerm << endl;
	 }
      } else if (string((char*)node->name) == "NetOutput")
      {
	 char *str_termName = (char *)xmlGetProp(node, (CHAR *)"name");
	 char *str_termTerm = (char *)xmlGetProp(node, (CHAR *)"terminal");
	 char *str_termNode = (char *)xmlGetProp(node, (CHAR *)"node");
	 string termName = string(str_termName);
	 string termTerm = string(str_termTerm);
	 string termNode = string(str_termNode);
	 free(str_termName); free(str_termTerm); free(str_termNode);
	 
	 if (getNodeNamed(termNode) && getNodeNamed(termNode)->getOutputNamed(termTerm))
	 {
	    newNetTerminal(getNodeNamed(termNode)->getOutputNamed(termTerm),
			   UINetTerminal::OUTPUT, termName);
	 } else {
	    cerr << "Invalid netTerminal at " << termNode << ":" << termTerm << endl;
	 }
      } else if (string((char*)node->name) == "NetCondition")
      {
	 char *str_termName = (char *)xmlGetProp(node, (CHAR *)"name");
	 char *str_termTerm = (char *)xmlGetProp(node, (CHAR *)"terminal");
	 char *str_termNode = (char *)xmlGetProp(node, (CHAR *)"node");
	 string termName = string(str_termName);
	 string termTerm = string(str_termTerm);
	 string termNode = string(str_termNode);
	 free(str_termName); free(str_termTerm); free(str_termNode);

	 if (getNodeNamed(termNode) && getNodeNamed(termNode)->getOutputNamed(termTerm))
	 {
	    newNetTerminal(getNodeNamed(termNode)->getOutputNamed(termTerm),
			   UINetTerminal::CONDITION, termName);
	 } else {
	    cerr << "Invalid netTerminal at " << termNode << ":" << termTerm << endl;
	 }
      }
      node = node->next;
   }
   //cerr << "done...\n";

}



UINetwork::~UINetwork() 
{
   if (!destroyed)
   {
      destroyed=true;
      //Links are deleted through the nodes destructor
      for (int i=0;i<nodes.size();i++)
	 delete nodes[i];
      for (int i=0;i<terminals.size();i++)
	 delete terminals[i];
   }
}

UINode *UINetwork::loadNode (xmlNodePtr node)
{
   //return NULL;
   //cerr << "adding node in UINetwork::loadNode\n";
   UINode *theNewNode = newNode (this, node);
   //cerr << "node created\n";
   nodes.insert(nodes.end(), theNewNode);
   return theNewNode;
}

UINode *UINetwork::getNodeNamed(string n)
{
   for (int i=0;i<nodes.size();i++)
      if (nodes[i]->getName() == n)
         return nodes[i];
   return NULL;
}

void UINetwork::addNode(UINode *node)
{
    nodes.insert(nodes.end(), node);
}


void UINetwork::removeNode(UINode *node) 
{
   if (destroyed)
      return;
   //Should comply with ANSI C++
   vector<UINode *>::iterator i=nodes.begin();
   while (i != nodes.end())
   {
      if (*i == node)
      {
	 nodes.erase(i);
	 break;
      }
      ++i;
   }
   /*for (int i=0;i<nodes.size();i++)
      if (nodes[i]==node)
         nodes.erase(&nodes[i]);*/
   doc->setModified();
   /*if (node == conditionNode)
     conditionNode = NULL;*/
}

void UINetwork::addLink(UILink *link)
{
   links.insert(links.end(), link);
   doc->setModified();
}

void UINetwork::removeLink(UILink *link) 
{
   //Fix to be ANSI C++
   vector<UILink *>::iterator i=links.begin();
   while (i != links.end())
   {
      if (*i == link)
      {
	 links.erase(i);
	 break;
      }
      ++i;
   }
   /*for (int i=0;i<links.size();i++)
      if (links[i]==link)
         links.erase(&links[i]);*/
   doc->setModified();
}



void UINetwork::saveXML(xmlNode *root)
{
   xmlNodePtr tree;
   tree = xmlNewChild(root, NULL, (CHAR *)"Network", NULL);
   switch (type)
   {
      case subnet:
     xmlSetProp(tree, (CHAR *)"type", (CHAR *)"subnet");
     break;
      case iterator:
     xmlSetProp(tree, (CHAR *)"type", (CHAR *)"iterator");
     break;
      case threaded:
     xmlSetProp(tree, (CHAR *)"type", (CHAR *)"threaded");
     break;
   }
   xmlSetProp(tree, (CHAR *)"name", (CHAR *)name.c_str());
   /*if (isIterator && conditionNode)
     xmlSetProp(tree, (CHAR *)"condition", (CHAR *)conditionNode->getName().c_str());*/
   for (int i=0;i<nodes.size();i++)
   {
      nodes[i]->saveXML(tree);
   }
   for (int i=0;i<links.size();i++)
   {
      links[i]->saveXML(tree);
   }
   for (int i=0;i<terminals.size();i++)
   {
      terminals[i]->saveXML(tree);
   }
 

}

void UINetwork::setModified() 
{
   doc->setModified();
   //Not a good idea at all... 
   //interfaceChangeNotify();
}

vector<string> UINetwork::getTerminals(UINetTerminal::NetTermType termType)
{
   vector<string> term;
   for (int i=0;i<terminals.size();i++)
   {
      UINetTerminal::NetTermType type = terminals[i]->getType();
      if (type == termType)
     term.insert(term.end(), terminals[i]->getName());
   }
   return term;
}

void UINetwork::newNetNotify(const string &cat, const string &type) 
{
   //if (type != name) 
   //   popup->addType(cat,type);
}

void UINetwork::insertNetParams(vector<ItemInfo *> &params)
{
   for (int i=0;i<nodes.size();i++)
      nodes[i]->insertNetParams(params);
   if (type == iterator) 
      {
       ItemInfo *newInfo = new ItemInfo;
        newInfo->name = "DOWHILE";
        params.insert(params.end(), newInfo);
      }
      // params.insert(params.end(), "DOWHILE");
   if (type == threaded)
      {
            ItemInfo *newInfo = new ItemInfo;
            newInfo->name = "RATE_PER_SECOND";
            params.insert(params.end(), newInfo);   
      }
      // params.insert(params.end(), "RATE_PER_SECOND");
}

UINode *UINetwork::newNode(UINetwork* _net, string _name, string _type, 
                           double _x, double _y, bool doInit)
{
    return new UINode(_net, _name, _type, _x, _y, doInit);
}

UINode *UINetwork::newNode(UINetwork* _net, xmlNodePtr def)
{
   //cerr << "UINetwork::newNode\n";

   return new UINode(_net, def);
}

/*UITerminal *UINetwork::newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y)
{
   return new UITerminal (_name, _node, _isInput, _x, _y);
}*/

UILink *UINetwork::newLink (UITerminal *_from, UITerminal *_to, char *str)
{
   //BUG HERE (GUI instead of UI)
   //cerr << "UINetwork::newLink\n";
   return new UILink (_from, _to, str);
}

UINetTerminal *UINetwork::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name)
{
   //BUG HERE
   //cerr << "UINetwork::newNetTerminal\n";
   return new UINetTerminal (_terminal, _type, _name);
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
      case threaded:
	 net = new ThreadedIterator(netName, params);
	 break;
   }


   try 
   {

      for (int i=0;i<nodes.size();i++)
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
   for (int i=0;i<links.size();i++)
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
   for (int i=0;i<terminals.size();i++)
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
   for (int i=0;i<terminals.size();i++)
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
   for (int i=0;i<nodes.size();i++)
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
   for (int i=0;i<ids.size();i++)
   {
      out << "   aNode = genNode" << ids[i] << "(params);\n";
      out << "   net->addNode(*aNode);\n\n";
   }


   for (int i=0;i<links.size();i++)
   {
      links[i]->genCode(out);
   }



   bool inputExist=false;
   for (int i=0;i<terminals.size();i++)
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
   for (int i=0;i<terminals.size();i++)
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

void UINetwork::addTerminal(UINetTerminal *term) 
{
   terminals.insert(terminals.end(), term);
   //Shouldn't do it here because at this point of the netTerminal creation, there's no name assigned yet.
   //interfaceChangeNotify();
}

void UINetwork::removeTerminal(UINetTerminal *term) 
{
   vector<UINetTerminal *>::iterator i=terminals.begin();
   while (i != terminals.end())
   {
      if (*i == term)
      {
	 terminals.erase(i);
	 break;
      }
      ++i;
   }
   interfaceChangeNotify();
}

void UINetwork::interfaceChangeNotify()
{
   if (!destroyed)
      doc->updateNetInfo(this);
}

void UINetwork::rename(string newName)
{
  
  if (doc->getNetworkNamed(newName)) {
    throw new GeneralException(string("Network name already exist : ")+ newName, __FILE__,__LINE__);
  }


   //FIXME: We should update the subnet list and rename the node that 
   //correspond to this subnet
  string oldName = name;



  name = newName;
  doc->updateAllNetworks();
  
   
  //update node information (type changed (subnet name))
  //best way to do that?
  
  vector<UINetwork*> my_nets = doc->get_networks();
  
  for (int i = 0; i < my_nets.size(); i++) {
    
    vector<UINode *> my_nodes = my_nets[i]->getNodes();
    
    for (int j = 0; j < my_nodes.size(); j++) {
   
      if (my_nodes[j]->getType() == oldName) {

	my_nodes[j]->rename(newName);
	
      }
    }
  }

}
