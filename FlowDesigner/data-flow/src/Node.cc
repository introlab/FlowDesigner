// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "Node.h"
#include <string>
#include <typeinfo>
#include "Object.h"

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
   , processCount(-1)
   , outputInitializeCount (0)
   , parameters(params)
   , exit_status(false)
   , uinode(NULL)
{
#ifdef MULTITHREAD
   pthread_mutex_init(&mutex, NULL);
#endif
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
      //the old method was used for adding inputs
      throw new NodeException(this, "We shouldn't be initializing this way anymore", __FILE__, __LINE__);
      inputs.resize(in + 1);
      inputs[in] = NodeInput(inputNode,out,string());
   }
   else {
      //the new method was used for adding inputs
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

   for (int in = 0; in < outputNames.size(); in++) {
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
   return (output_id < outputNames.size());
}
/***************************************************************************/
/*
  translateOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
int Node::translateOutput (string output_name) {

   //We should check if the output exists
   for (int in = 0; in < outputNames.size(); in++) {
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
   
   for (int in = 0; in < inputs.size(); in++) {
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
         if (!in->node || in->outputID == -1) {
            throw new NodeException(this, "The node is not properly connected",__FILE__,__LINE__);
         }
         else {
            if (!in->node->hasOutput(in->outputID)) 
               throw new NodeException(this, "Input node doesn't implement output", __FILE__, __LINE__);
	    try {
            in->node->initialize();
	    } catch (BaseException *e)
	    {
	       throw e->add(new NodeException(this, "Exception caught in node initialization", __FILE__, __LINE__));
	    }
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
   processCount = -1;
}
/***************************************************************************/
/*
  reset()
  Jean-Marc Valin
 */
/***************************************************************************/
void Node::reset()
{
   processCount=-1;
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
      cerr << "The factory already exists. Ignoring the new one." << endl;
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
void Node::rt_assert(bool cond, string message="", char *_file, int _line)
{
   if (cond)
      return;
   throw_error(true, message, _file, _line);
}

/**Init-time assertions*/
void Node::construct_assert(bool cond, string message="", char *_file, int _line)
{
   throw_error(false, message, _file, _line);
}

/**Error with the node*/
void Node::throw_error(bool send_ptr, string message, char *_file, int _line)
{
   throw new NodeException (send_ptr ? this : NULL,message,_file,_line);
}


//} //namespace DataFlow
