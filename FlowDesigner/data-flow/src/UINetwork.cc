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



UINetwork::UINetwork(UIDocument *_doc, string _name, Type _type)
   //: canvas(doc->getCanvas())
   : doc(_doc)
   , name(_name)
   , type (_type)
   , destroyed(false)
   //, conditionNode(NULL)
{
   
   //create();
}

UINetwork::UINetwork(UIDocument *_doc, xmlNodePtr net, bool init)
   : doc(_doc)
   , destroyed(false)
   //, conditionNode(NULL)
{
   if (init)
      load(net);
}

void UINetwork::load (xmlNodePtr net)
{
   name = string((char *)xmlGetProp(net, (CHAR *)"name"));

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
   //name(_name)
   //create();

   //cerr << "parsing nodes\n";
   xmlNodePtr node = net->childs;
   while (node != NULL)
   {
      if (string((char*)node->name) == "Node")
      {
         UINode *theNewNode = loadNode (node);

         /*if (condNode && theNewNode->getName() == condNode)
         {
            //cerr << "found the cond node\n";
            conditionNode == theNewNode;
            theNewNode->setAsCondition();
        }*/
      }
      node = node->next;
   }

   //cerr << "parsing links\n";
   node = net->childs;
   while (node != NULL)
   {
      if (string((char*)node->name) == "Link")
      {
         string fromnode = string((char *)xmlGetProp(node, (CHAR *)"from"));
         string out = string((char *)xmlGetProp(node, (CHAR *)"output"));
         string tonode = string((char *)xmlGetProp(node, (CHAR *)"to"));
         string in = string((char *)xmlGetProp(node, (CHAR *)"input"));
         /*cerr << fromnode << ":" << out << " -> " << tonode << ":" << in << endl;
         cerr << getNodeNamed(fromnode)->getOutputNamed(out) << " "
         << getNodeNamed(tonode)->getInputNamed(in) << endl;*/

     //BUG HERE
         newLink(getNodeNamed(fromnode)->getOutputNamed(out),
                  getNodeNamed(tonode)->getInputNamed(in));
      }
      node = node->next;
   }

   //cerr << "parsing netTerminals\n";
   node = net->childs;
   while (node != NULL)
   {
      if (string((char*)node->name) == "NetInput")
      {
     //cerr << "in\n";
         string termName = string((char *)xmlGetProp(node, (CHAR *)"name"));
         string termTerm = string((char *)xmlGetProp(node, (CHAR *)"terminal"));
         string termNode = string((char *)xmlGetProp(node, (CHAR *)"node"));
         newNetTerminal(getNodeNamed(termNode)->getInputNamed(termTerm),
                 UINetTerminal::INPUT, termName);
      } else if (string((char*)node->name) == "NetOutput")
      {
     //cerr << "out\n";
         string termName = string((char *)xmlGetProp(node, (CHAR *)"name"));
         string termTerm = string((char *)xmlGetProp(node, (CHAR *)"terminal"));
         string termNode = string((char *)xmlGetProp(node, (CHAR *)"node"));
     //BUG HERE
     //cerr << termName << " " << termTerm << " " << termNode << endl;
     //cerr << getNodeNamed(termNode)->getName() << endl;
     //cerr << getNodeNamed(termNode)->getOutputNamed(termTerm) << endl;
         newNetTerminal(getNodeNamed(termNode)->getOutputNamed(termTerm),
                 UINetTerminal::OUTPUT, termName);
     //cerr << "--\n";
      } else if (string((char*)node->name) == "NetCondition")
      {
         string termName = string((char *)xmlGetProp(node, (CHAR *)"name"));
         string termTerm = string((char *)xmlGetProp(node, (CHAR *)"terminal"));
         string termNode = string((char *)xmlGetProp(node, (CHAR *)"node"));
     //BUG HERE
         newNetTerminal(getNodeNamed(termNode)->getOutputNamed(termTerm),
                 UINetTerminal::CONDITION, termName);
      }
      node = node->next;
   }
   //cerr << "done...\n";

}



UINetwork::~UINetwork() 
{
   //cerr << "destroying UINetwork " << name << endl;
   if (!destroyed)
   {
      /*for (int i=0;i<nodes.size();i++)
    delete nodes[i];*/
      //although this is wierd, it has to be like that since the destroyed link removes 
      //itself from the connection list
      while (nodes.size())
     delete nodes[0];

/*
  there shouldn't be any links left...
  for (int i=0;i<links.size();i++)
  delete links[i];
*/

   }
   //delete popup;
   //gtk_object_destroy(GTK_OBJECT(group));
   //gtk_widget_destroy(GTK_WIDGET(canvas));
}

UINode *UINetwork::loadNode (xmlNodePtr node)
{
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

void UINetwork::setModified() {doc->setModified();}

void UINetwork::export2net(ostream &out)
{
   if (type == iterator)
      out << "Iterator: ";
   else
      out << "Network: ";
   out << name << "\n{\n";

   bool inputExist=false;
   bool outputExist=false;
   bool condExist=false;
   for (int i=0;i<terminals.size();i++)
   {
      UINetTerminal::NetTermType type = terminals[i]->getType();
      if (type == UINetTerminal::INPUT)
         inputExist=true;
      if (type == UINetTerminal::OUTPUT)
         outputExist=true;
      if (type == UINetTerminal::CONDITION)
         condExist=true;
   }

   if (inputExist)
      out << "   <node: ALL_NETWORK_INPUTS> <type: Collector>" << endl << endl;

   for (int i=0;i<nodes.size();i++)
   {
      nodes[i]->export2net(out);
   }
   
   if (outputExist)
   {
      out << "   <node: ALL_NETWORK_OUTPUTS> <type: Collector>" << endl;
      for (int i=0;i<terminals.size();i++)
      {
         UINetTerminal::NetTermType type = terminals[i]->getType();
         if (type == UINetTerminal::OUTPUT)
            out << "   <input: " << terminals[i]->getName() << ", " 
                << terminals[i]->getTerminal()->getNode()->getName() << ", "
                << terminals[i]->getTerminal()->getName() << ">" << endl;
      }
      out << endl;
   }

   if (condExist)
   {
      out << "   <node: ITERATOR_CONDITION> <type: Collector>" << endl;
      for (int i=0;i<terminals.size();i++)
      {
         UINetTerminal::NetTermType type = terminals[i]->getType();
         if (type == UINetTerminal::CONDITION)
            out << "   <input: OUTPUT, " 
                << terminals[i]->getTerminal()->getNode()->getName() << ", "
                << terminals[i]->getTerminal()->getName() << ">" << endl;
      }
      out << endl;
   }

   if (inputExist)
      out << "   <netInput: ALL_NETWORK_INPUTS>" << endl;

   if (outputExist)
      out << "   <netOutput: ALL_NETWORK_OUTPUTS>" << endl;

   if (condExist)
      out << "   <netCondition: ITERATOR_CONDITION>" << endl;
   /*if (conditionNode != NULL)
     out << "   <netCondition: " << conditionNode->getName() << ">" << endl;*/
   out << "\n}\n\n";
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

/*void UINetwork::setCondition(UINode *cond)
{
   if (!isIterator)
   {
      cerr << "Subnets don't have conditions\n";
      return;
   }

   if (conditionNode != NULL)
      conditionNode->unsetAsCondition();
   conditionNode=cond;
   }*/

void UINetwork::newNetNotify(const string &cat, const string &type) 
{
   //if (type != name) 
   //   popup->addType(cat,type);
}

void UINetwork::insertNetParams(vector<ItemInfo *> &params)
{
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
   for (int i=0;i<nodes.size();i++)
      nodes[i]->insertNetParams(params);
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

UILink *UINetwork::newLink (UITerminal *_from, UITerminal *_to)
{
   //BUG HERE (GUI instead of UI)
   //cerr << "UINetwork::newLink\n";
   return new UILink (_from, _to);
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

   for (int i=0;i<nodes.size();i++)
   {
      //cerr << "building node " << nodes[i]->getName() << endl;
      Node *aNode = nodes[i]->build(params);
      //cerr << "adding node " << nodes[i]->getName() << endl;
      //cerr << aNode << endl;
      net->addNode(*aNode);
   }

   //cerr << "nodes built\n";

   //insert links
   for (int i=0;i<links.size();i++)
   {
      //cerr << "linking...\n";
      links[i]->build(net);
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
   
   return net;
}