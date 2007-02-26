// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Node.h"
#include <string>
#include <typeinfo>
#include "Object.h"
#include <sstream>


/** \mainpage FlowDesignerCore FlowDesigner User Guide


    Testing.
    \ref FlowDesignerCore ["FlowDesigner Core"]

*/




//@implements core

using namespace std;

namespace FD {

//our static factory dictionary
map<string,_NodeFactory*> &Node::factoryDictionary()
{ 
   static map<string,_NodeFactory*> var = map<string,_NodeFactory*>();
   return var; 
}

//our static node info dictionary
vector<string> &Node::nodeInfo()
{ 
   static vector<string> var = vector<string>();
   return var; 
}

//our static xpm dictionary
map<string, char**> &Node::XPMDictionary()
{
  static map<string,char**> var = map<string,char**>();
  return var;
}

//map<string,_NodeFactory*> Node::factoryDictionary;
/***************************************************************************/
/*
  Node(...)
  Jean-Marc Valin
 */
/***************************************************************************/
Node::Node(string nodeName, const ParameterSet &params) 
   : name (nodeName)
   , inputs (vector<NodeInput>(0))
   , parameters(params)
   , uinode(NULL)
{
}
/***************************************************************************/
/*
  connectToNode(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Node::connectToNode(unsigned int in, Node *inputNode, unsigned int out)
{
   if (inputs.size() <= in) {
     char message[256];
     sprintf(message,"Input %i doesn't exist",in,getName().c_str());
     throw new NodeException(this,message, __FILE__, __LINE__);
   }
   else {
      inputs[in].outputID = out;
      inputs[in].node = inputNode;
   }
}
/***************************************************************************/
/*
  addInput(...)
  Dominic Letourneau
 */
/***************************************************************************/
int Node::addInput (const string &input_name) {
   
   //We should check if the input already exists
   vector<NodeInput>::iterator iter;
   
   for (iter = inputs.begin(); iter < inputs.end(); iter++) {
      if (iter->name == input_name) {
         throw new NodeException(this,string("Input already defined : ") + input_name, __FILE__, __LINE__);
         //just in case
         break;
      }      
   }

   //Adding the input
   int position = inputs.size();
   inputs.resize(inputs.size() + 1);

   //Creating a new NodeInput.

   //cerr<<"Creating a new input named : "<<input_name<<endl;
   inputs[position] = NodeInput(input_name);

   return position;

}
/***************************************************************************/
/*
  addOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
int Node::addOutput(const string &output_name) {
   
   //We should check if the output already exists

   for (unsigned int in = 0; in < outputNames.size(); in++) {
      if (outputNames[in] == output_name) {
         throw new NodeException(this,string("Output already defined : ") + output_name, __FILE__, __LINE__);
         //just in case
         break;
      }      
   }
   int position = outputNames.size();
   outputNames.resize(outputNames.size() + 1);
   outputNames[position] = output_name;
   return position;
}


/***************************************************************************/
/*
  hasOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
bool Node::hasOutput (int output_id) const {
   return (output_id < int(outputNames.size()));
}


/***************************************************************************/
/*
  verifyConnect(...)
  Jean-Marc Valin
 */
/***************************************************************************/
void Node::verifyConnect()
{
   vector<NodeInput>::iterator in;
   for (in = inputs.begin(); in < inputs.end(); in++)
   {        
     if (!in->node || in->outputID == -1) {
       throw new NodeException(this, string("The node is not properly connected") + string(" input name : ") + in->name,__FILE__,__LINE__);
     }
      
     if (!in->node->hasOutput(in->outputID)) {
       throw new NodeException(this, string("The node is connected to an invalid output on node : ") + in->node->getName(),__FILE__, __LINE__);
     }
      
   }
   
}


/***************************************************************************/
/*
  translateOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
int Node::translateOutput (string output_name) {

   //We should check if the output exists
   for (unsigned int in = 0; in < outputNames.size(); in++) {
      if (outputNames[in] == output_name) {
         return in;
      }      
   }

   throw new NodeException(this,string("Unknown output in translateOutput : ") + output_name, __FILE__,__LINE__);
   //should never return...
   return -1;
}


/***************************************************************************/
/*
  translateInput(...)
  Dominic Letourneau
 */
/***************************************************************************/
int Node::translateInput (string input_name) {
   
   //We should check if the input exists
   
   for (unsigned int in = 0; in < inputs.size(); in++) {
      if (inputs[in].name == input_name) {
         return in;
      }      
   }
   throw new NodeException(this,string("Unknown input in translateInput : ") + input_name, __FILE__,__LINE__);
   return -1;
}


/***************************************************************************/
/*
  connectToNode()
  Jean-Marc Valin
 */
/***************************************************************************/
void Node::connectToNode(string in, Node *inputNode, string out)
{
   connectToNode(translateInput(in), inputNode, inputNode->translateOutput(out));
}


/***************************************************************************/
/*
  initialize(...)
  Jean-Marc Valin
 */
/***************************************************************************/
void Node::initialize()
{
}


/***************************************************************************/
/*
  reset()
  Jean-Marc Valin
 */
/***************************************************************************/
void Node::reset()
{
}


/***************************************************************************/
/*
  Node::getFactoryNamed (...)
  Dominic Letourneau
 */
/***************************************************************************/
_NodeFactory* Node::getFactoryNamed (const string &name) {

   _NodeFactory* factory = NULL;
   map<string,_NodeFactory*>::iterator iter;
   
   //let's find the key in our map
   //if not found will return NULL
   for (iter = factoryDictionary().begin(); iter != factoryDictionary().end(); iter++) {
      if ((*iter).first == name) {
         factory = (*iter).second;
         break;
      }
   }
   return factory;
}

/***************************************************************************/
/*
  Node::getXPM (...)
  Dominic Letourneau
 */
/***************************************************************************/
char** Node::getXPM (const string &nodeName) {
  
  char **result = NULL;
  
  map<string,char**>::iterator iter;
  
  //let's find the key in our map
  //if not found will return NULL
  for (iter = XPMDictionary().begin(); iter != XPMDictionary().end(); iter++) {
    if ((*iter).first == nodeName) {
      result = (*iter).second;
      break;
    }
  }
  return result;   
}

/***************************************************************************/
/*
  Node::addXPM()
  Dominic Letourneau
  We should not throw exceptions here, as there is no way to catch it if the
  toolbox is being dynamically loaded
 */
/***************************************************************************/
int Node::addXPM (const string &nodeName, char **XPMData) {
  if (getXPM(nodeName) == NULL) {
    XPMDictionary().insert(make_pair(nodeName,XPMData));
    return 0;
  }
  else {
    return -1;
  }

}

/***************************************************************************/
/*
  Node::addFactory()
  Dominic Letourneau
  We should not throw exceptions here, as there is no way to catch it if the
  toolbox is being dynamically loaded
 */
/***************************************************************************/
int Node::addFactory (const string &factoryName, _NodeFactory* const factory) {
   if (!getFactoryNamed(factoryName)) {
      //the factory doesn't exist inserting it...
      if (factory != NULL) {
#ifdef WIN32
#warning Please remove this debug line.      	
		 std::cerr<<"Inserting factory: "<<factoryName<<endl;
#endif		 
         factoryDictionary().insert (factoryEntry(factoryName,factory));
      }
      else {
         cerr<<"Node::addFactory : NULL _NodeFactory pointer, exiting"<<endl;
         exit(-1);
      }
   }
   else {
      cerr << "Node::addFactory : The factory (" << factoryName << ") already exists. Ignoring the new one." << endl;
   }
   return 0;
};


/***************************************************************************/
/*
  Node::addFactory()
  Jean-Marc Valin
 */
/***************************************************************************/
int Node::addNodeInfo (const string &info) {
   nodeInfo().insert(nodeInfo().end(),info);
   return 0;
};


/**Run-time assertions*/
void Node::rt_assert(bool cond, string message, const char *_file, int _line)
{
   if (cond)
      return;
   throw_error(true, message, _file, _line);
}

/**Init-time assertions*/
void Node::construct_assert(bool cond, string message, const char *_file, int _line)
{
   throw_error(false, message, _file, _line);
}

/**Error with the node*/
void Node::throw_error(bool send_ptr, string message, const char *_file, int _line)
{
   throw new NodeException (send_ptr ? this : NULL,message,_file,_line);
}

void Node::printOn(ostream &out) const
{
   out << "<Node" << endl;
   out << "<name " << name << " >" << endl;
   out << ">" << endl;
}





void NodeException::print(ostream &out) 
{
   if (frozen)
   {
      out << message;
   } else {
      if (node)
      {
	 out << file << " line " << line << ": Node " << node->getName() 
	     << " (type " << typeid(*node).name() << ") " << message << endl;
      } else {
	 out << file << ", line " << line << ": " << message << endl;
      }
   }
}

void NodeException::freeze()
{
   if (frozen)
      return;
   
   ostringstream outStr;
   print(outStr);
   message = outStr.str();
   frozen = true;
}

} //namespace FD
