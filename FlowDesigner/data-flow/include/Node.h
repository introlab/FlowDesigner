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

#ifndef _NETWORKNODE_H_
#define _NETWORKNODE_H_


#include <string>
#include <map>
#include <vector>
#include "Object.h"
#include "ObjectRef.h"
#include "Exception.h"
#include <typeinfo>
#include "ParameterSet.h"

#include "NodeFactory.h"
//class _NodeFactory;

#ifdef MULTITHREAD
#include <pthread.h>
#endif

//using namespace std;
//namespace DataFlow {

/**Definition of the type we need for the dictionaries*/
typedef map<string, Node*>::value_type nodeEntry;
typedef map<string, _NodeFactory*>::value_type factoryEntry; 


//must be defined
class Node;

/** A NodeInput is a data structure that holds a reference to
    the node we are connected at and its related output number.
    @author Jean-Marc Valin
    @version 1.0
*/
class NodeInput {
public:
   /**The outputID of the connected node*/
   int outputID;
   /**The reference of the node*/
   Node *node;
   /**The name of the input*/
   string name;
   /**Constructor with a node and an outputID*/
   NodeInput(Node *n, int t, const string &inputName) :outputID(t),node(n),name(inputName) {}
   /**Copy constructor*/
   NodeInput (const NodeInput &in) {
      node = in.node; 
      outputID = in.outputID;
      name = in.name;
   }
   /**equality operator*/
   NodeInput& operator= (const NodeInput &in) {
      node = in.node; 
      outputID = in.outputID; 
      name = in.name;
      return *this;
   }
   /**default constructor*/
   NodeInput() : outputID(-1), node(NULL) {} //-1 means unused

   /**constructor with a nodeName*/
   NodeInput(const string &inputName) : outputID(-1), node(NULL), name(inputName) {}

private:   
};


/**The Base Node class. All nodes to be inserted in a network must
   derive from this class. It contains the proper initializations
   for the connectivity of the nodes.
   @author Jean-Marc Valin & Dominic Letourneau
   @version 1.0
*/
class Node { 

   /**A network can have access to private members of Node*/
   friend class Network;
   friend class Iterator;

protected:

   bool exit_status;
   
   /**Node's name*/
   string name;

   /**Node's inputs*/
   vector<NodeInput> inputs;

   /**Node's outputs*/
   vector<string> outputNames;

   /**Whether the node has been initialized*/
   bool initialized;
   
   /**Is the node in debug Mode*/
   bool debugMode;

   /**Internal processing counter for synchronization.
      This counter is used to find out whether the output of the node
      needs to be updated */
   int processCount;
   
   /**Used during initialization.
      Becomes zero when all the node's outputs have been initialized*/
   int outputInitializeCount;
   
   /**Node's status*/
   long status;
   
   /**Parameters given to the node at construction time*/
   ParameterSet parameters;

   /**Connect an input node using numeric (integer) input/output names*/
   virtual void connectToNode(unsigned int in, Node *inputNode, unsigned int out);

   /**Adding an output to a node*/
   virtual int addOutput (const string &outputName);
 
   /**Adding an input to a node*/
   virtual int addInput (const string &inputName);
   
#ifdef MULTITHREAD
   /**pthread mutex*/
   pthread_mutex_t mutex;
#endif

   /**Returns the inputs vector */
   virtual vector<NodeInput>& getInputs () {return inputs;}

public:

   /**Constructor, takes the name of the node and a set of parameters*/
   Node(string nodeName, const ParameterSet &params);
   

   /**Destructor*/
   virtual ~Node() {}

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count) = 0; 

   /**Ask for the node's output (named) and for the count iteration */
   virtual ObjectRef getOutputNamed (const string &outputName, int count) {
      return this->getOutput (this->translateOutput(outputName),count);
   }

   /**Connect an input node using symbolic (strings) input/output names*/
   virtual void connectToNode(string in, Node *inputNode, string out);

   /**Initialize a node*/
   virtual void initialize ();

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Checks whether node really has a certain output*/
   virtual bool hasOutput(int output_id) const;

   /**Has the node been initialized?*/
   bool isInitialized() {return initialized;}

   /**Is the node in debug mode?*/
   bool isDebugMode() {return debugMode;}

   ObjectRef getInput(int inputID, int count)
   {
      NodeInput input = inputs[inputID];
      return input.node->getOutput(input.outputID, count);
   }

   /**Sets the node to debug mode*/
   virtual void setDebugMode(){debugMode = true;}

   virtual void setExitStatus(){exit_status = true;}

   bool isExitStatus() {return exit_status;}

   virtual void resetExitStatus() {exit_status = false;}

   /**Resets debug mode*/
   virtual void resetDebugMode(){debugMode = false;}

   /**Resets the node internal values and buffers*/
   virtual void reset();

   /**Returns the node name*/
   string getName() {return name;}
 
   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req) {}

   /**Standard request-passing method between nodes during initialization
    This one will be removed*/
   //virtual void request(const ParameterSet &req) {}

   /**locks the node's mutex*/
   void lock() 
   { 
#ifdef MULTITHREAD
      pthread_mutex_lock(&mutex); 
#endif
   }
   
   /**unlocks the node's mutex*/
   void unlock() 
   { 
#ifdef MULTITHREAD
      pthread_mutex_unlock(&mutex);
#endif
   }

#ifdef MULTITHREAD
   template <class T>
   T unlock_and_return(T &var)
   {
      T tmp = var;
      unlock();
      return tmp;
   }
#else
   template <class T>
   T &unlock_and_return(T &var)
   {
      return var;
   }
#endif

   /**Adding a factory into the static dictionary*/
   static int addFactory (const string &factoryName, _NodeFactory* const factory);

   /**The factory lookup function*/
   static _NodeFactory* getFactoryNamed (const string &name);

private:
   /**Tell the node we will be using output 'out'*/
   void registerOutput (int out) {outputInitializeCount++;}

   /**Increment outputInitializeCount when performing a reset()*/
   void incrementOutputInitialize() {outputInitializeCount++;}


protected:

   /**Default constructor, should not be used*/
   Node() {throw new GeneralException("Node Constructor should not be called",__FILE__,__LINE__);}

   /**symbolic to numeric translation for input names*/
   virtual int translateInput(string inputName);

   /**symbolic to numeric translation for output names*/
   virtual int translateOutput(string inputName);

public:
   /**The node instance factory*/
   static map<string,_NodeFactory*> &factoryDictionary();

   /**The node information map*/
   static vector<string> &nodeInfo();
   
   /**Routine to add info for a node*/
   static int addNodeInfo (const string &info);
};

/** Node Class methods/data
    @author Jean-Marc Valin
*/
class NodeClass {
protected:
public:
   _NodeFactory &factory();
   const string &name();
   const vector<string> &inputNames();
   const vector<string> &outputNames();
   const vector<pair<string,string> > &paramNames();
};

/***************************************************************************/
/*
  NotInitializedException
  Dominic Letourneau
 */
/***************************************************************************/
/** The NotInitializedException occurs when a node is not properly 
    initialized before the processing begins. It happens when the network
    is not properly connected.
    @author Dominic Letourneau
    @version 1.0
 */
class NotInitializedException : public BaseException {

public:
   /**The constructor that takes a map of nodes not properly initialized*/
   NotInitializedException (map<string,Node * > aMap) {
      nodeMap = aMap;
   }
   /**The print method*/
   virtual void print(ostream &out = cerr) {
      out<<"NotInitializedException occured"<<endl;
      
      map<string,Node*>::iterator iter;
      
      for (iter = nodeMap.begin(); iter != nodeMap.end(); iter++) {
         out<<"This node is not initialized: "<<(*iter).first<<endl;
      }
   }   

   /**The node map*/
   map<string,Node*> nodeMap;
};

/** The NodeException is a easy way to send a message for an general 
    exception in a node. You should use __FILE__ and __LINE__ in the 
    arguments.
    @author Jean-Marc Valin
    @version 1.0
*/
class NodeException : public BaseException {

public:

   /**The constructor with a message a file name and a line number*/
   NodeException( Node *_node, string _message, char *_file, int _line) 
      : message(_message)
      , node(_node)
      , file(_file)
      , line(_line)
   {}   
   /**the print method*/
   virtual void print(ostream &out = cerr) 
   {
      if (node)
         out << file << " line " << line << ": Node " << node->getName() 
             << " (type " << typeid(*node).name() << ") " << message << endl;
      else out << file << ", line " << line << ": " << message << endl;
   }
protected:
   /**the message*/
   string message;
   /**the node pointer*/
   Node *node;
   /**the file name*/
   string file;
   /**the line number*/
   int line;
};

#define DECLARE_NODE(NodeTypeName) int dummy_initializer_for ## NodeTypeName = \
               Node::addFactory (# NodeTypeName, new NodeFactory<NodeTypeName>(# NodeTypeName));


//} //namespace DataFlow 

#endif
