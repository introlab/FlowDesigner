// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Node.h"
#include <string>
#include <typeinfo>
#include "Object.h"
#include <sstream>

//@implements core

using namespace std;

//using namespace DataFlow;
//namespace DataFlow {

//our static factory dictionary
map<string,_NodeFactory*> &Node::factoryDictionary()
{ 
   static map<string,_NodeFactory*> var;
   return var; 
}

//our static node info dictionary
vector<string> &Node::nodeInfo()
{ 
   static vector<string> var;
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
   , initialized (false)
   , outputInitializeCount (0)
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
      throw new NodeException(this, "Input doesn't exist", __FILE__, __LINE__);
   }
   else {
      inputs[in].outputID = out;
      inputs[in].node = inputNode;
      inputNode->registerOutput(out);
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
      if (!in->node || in->outputID == -1)
	 throw new NodeException(this, "The node is not properly connected",__FILE__,__LINE__);
      
      if (!in->node->hasOutput(in->outputID))
	 throw new NodeException(this, "Input node doesn't implement output", __FILE__, __LINE__);
      
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
  initialize()
  Jean-Marc Valin & Dominic Letourneau
 */
/***************************************************************************/
void Node::initialize ()
{
   if (initialized) return;
   if (--outputInitializeCount <=0)
   {
      specificInitialize();
      //parameters.checkUnused();

      vector<NodeInput>::iterator in;
      for (in = inputs.begin(); in < inputs.end(); in++)
      {        
	 try {
            in->node->initialize();
	 } catch (BaseException *e)
	 {
	    throw e->add(new NodeException(this, "Exception caught in node initialization", __FILE__, __LINE__));
	 }
         
      }
      
   }
}


/***************************************************************************/
/*
  specificInitialize(...)
  Jean-Marc Valin
 */
/***************************************************************************/
void Node::specificInitialize()
{
   initialized = true;
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
         factoryDictionary().insert (factoryEntry(factoryName,factory));
      }
      else {
         cerr<<"NULL _NodeFactory pointer, exiting"<<endl;
         exit(-1);
      }
   }
   else {
      //cerr << "The factory (" << factoryName << ") already exists. Ignoring the new one." << endl;
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
void Node::rt_assert(bool cond, string message, char *_file, int _line)
{
   if (cond)
      return;
   throw_error(true, message, _file, _line);
}

/**Init-time assertions*/
void Node::construct_assert(bool cond, string message, char *_file, int _line)
{
   throw_error(false, message, _file, _line);
}

/**Error with the node*/
void Node::throw_error(bool send_ptr, string message, char *_file, int _line)
{
   throw new NodeException (send_ptr ? this : NULL,message,_file,_line);
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




//} //namespace DataFlow
