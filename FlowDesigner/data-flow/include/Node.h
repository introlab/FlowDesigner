// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef _NODE_H_
#define _NODE_H_


#include <string>
#include <map>
#include <vector>
#include "Object.h"
#include "ObjectRef.h"
#include "BaseException.h"
#include <typeinfo>
#include "ParameterSet.h"

#include "NodeFactory.h"


/**Definition of the type we need for the dictionaries*/
typedef map<string, Node*>::value_type nodeEntry;
typedef map<string, _NodeFactory*>::value_type factoryEntry; 


//must be defined
class Node;

/** A NodeInput is a data structure that holds a reference to
    the node we are connected at and its related output number.
    @author Jean-Marc Valin
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
      if (&in != this)
      {
	 node = in.node; 
	 outputID = in.outputID; 
	 name = in.name;
      }
      return *this;
   }
   /**default constructor*/
   NodeInput() : outputID(-1), node(NULL) {} //-1 means unused

   /**constructor with a nodeName*/
   NodeInput(const string &inputName) : outputID(-1), node(NULL), name(inputName) {}

private:   
};

class UINode;

/**The Base Node class. All nodes to be inserted in a network must
   derive from this class. It contains the proper initializations
   for the connectivity of the nodes.
   @author Jean-Marc Valin & Dominic Letourneau
*/
class Node : public Object {

   /**A network can have access to private members of Node*/
   friend class Network;
   friend class Iterator;

protected:
   
   /**Node's name*/
   string name;

   /**Node's inputs*/
   vector<NodeInput> inputs;

   /**Node's outputs*/
   vector<string> outputNames;

   /**Whether the node has been initialized*/
   bool initialized;
      
   /**Used during initialization.
      Becomes zero when all the node's outputs have been initialized*/
   int outputInitializeCount;
      
   /**Parameters given to the node at construction time*/
   ParameterSet parameters;

   /**Corresponding UINode*/
   UINode *uinode;

   /**Connect an input node using numeric (integer) input/output names*/
   virtual void connectToNode(unsigned int in, Node *inputNode, unsigned int out);

   /**Adding an output to a node*/
   virtual int addOutput (const string &outputName);
 
   /**Adding an input to a node*/
   virtual int addInput (const string &inputName);
   

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

   ObjectRef getInput(int inputID, int count)
   {
      NodeInput &input = inputs[inputID];
      return input.node->getOutput(input.outputID, count);
   }
   
   /**Notify the node that is will be destroyed shortly*/
   virtual void cleanupNotify() {}

   /**Resets the node internal values and buffers*/
   virtual void reset();

   /**Returns the node name*/
   string getName() {return name;}
 
   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req) {}
   
   /**Verify input connections for the node*/
   virtual void verifyConnect();

   /**A node can print itself*/
   void printOn(ostream &out=cout) const;

   void setUINode(UINode *_uinode) {uinode = _uinode;}
   
   /**Adding a factory into the static dictionary*/
   static int addFactory (const string &factoryName, _NodeFactory* const factory);

   /**The factory lookup function*/
   static _NodeFactory* getFactoryNamed (const string &name);

protected:
   /**Tell the node we will be using output 'out'*/
   virtual void registerOutput (int out) {incrementOutputInitialize();}

   /**Increment outputInitializeCount when performing a reset()*/
   virtual void incrementOutputInitialize() {outputInitializeCount++;}


protected:

   /**Default constructor, should not be used*/
   Node() {throw new GeneralException("Node Constructor should not be called",__FILE__,__LINE__);}

   /**symbolic to numeric translation for input names*/
   virtual int translateInput(string inputName);

   /**symbolic to numeric translation for output names*/
   virtual int translateOutput(string inputName);

   /**Run-time assertions*/
   virtual void rt_assert(bool cond, string message="", char *_file="unknown", int _line=0);

   /**Construct-time assertions*/
   virtual void construct_assert(bool cond, string message="", char *_file="unknown", int _line=0);

   /**Error with the node*/
   virtual void throw_error(bool send_ptr, string message, char *_file, int _line);

public:
   /**The node instance factory*/
   static map<string,_NodeFactory*> &factoryDictionary();

   /**The node information map*/
   static vector<string> &nodeInfo();
   
   /**Routine to add info for a node*/
   static int addNodeInfo (const string &info);
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
*/
class NodeException : public BaseException {

public:

   /**The constructor with a message a file name and a line number*/
   NodeException( Node *_node, string _message, char *_file, int _line) 
      : message(_message)
      , node(_node)
      , file(_file)
      , line(_line)
      , frozen(false)
   {}   
   /**the print method*/
   virtual void print(ostream &out = cerr);

   virtual void freeze();

protected:
   /**the message*/
   string message;

   /**the node pointer*/
   Node *node;

   /**the file name*/
   string file;

   /**the line number*/
   int line;

   /**Whether the exception is frozen*/
   bool frozen;

};



#define DECLARE_NODE(NodeTypeName) int dummy_initializer_for ## NodeTypeName = \
               Node::addFactory (# NodeTypeName, new NodeFactory<NodeTypeName>(# NodeTypeName));



#endif
