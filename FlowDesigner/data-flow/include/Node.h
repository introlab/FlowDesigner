// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef _NODE_H_
#define _NODE_H_

#include "Object.h"
#include <string>
#include <map>
#include <vector>
#include "ObjectRef.h"
#include "BaseException.h"
#include <typeinfo>
#include "ParameterSet.h"
#include "NodeFactory.h"

namespace FD {

/**Definition of the type we need for the dictionaries*/
typedef std::map<std::string, Node*>::value_type nodeEntry;
typedef std::map<std::string, _NodeFactory*>::value_type factoryEntry; 


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
   std::string name;
   /**Constructor with a node and an outputID*/
   NodeInput(Node *n, int t, const std::string &inputName) :outputID(t),node(n),name(inputName) {}
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
   NodeInput(const std::string &inputName) : outputID(-1), node(NULL), name(inputName) {}

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
   std::string name;

   /**Node's inputs*/
   std::vector<NodeInput> inputs;

   /**Node's outputs*/
   std::vector<std::string> outputNames;

   /**Parameters given to the node at construction time*/
   ParameterSet parameters;

   /**Corresponding UINode*/
   UINode *uinode;

   /**Connect an input node using numeric (integer) input/output names*/
   virtual void connectToNode(unsigned int in, Node *inputNode, unsigned int out);

   /**Adding an output to a node*/
   virtual int addOutput (const std::string &outputName);
 
   /**Adding an input to a node*/
   virtual int addInput (const std::string &inputName);
   

   /**Returns the inputs vector */
   virtual std::vector<NodeInput>& getInputs () {return inputs;}

public:

   /**Constructor, takes the name of the node and a set of parameters*/
   Node(std::string nodeName, const ParameterSet &params);
   

   /**Destructor*/
   virtual ~Node() {}

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count) = 0; 

   /**Ask for the node's output (named) and for the count iteration */
   virtual ObjectRef getOutputNamed (const std::string &outputName, int count) {
      return this->getOutput (this->translateOutput(outputName),count);
   }

   /**Connect an input node using symbolic (strings) input/output names*/
   virtual void connectToNode(std::string in, Node *inputNode, std::string out);

   /**Initialize a node*/
   virtual void initialize ();

   /**Checks whether node really has a certain output*/
   virtual bool hasOutput(int output_id) const;

   ObjectRef getInput(int inputID, int count)
   {
      NodeInput &input = inputs[inputID];
      return input.node->getOutput(input.outputID, count);
   }

   /**Asks the node to stop what it is doing to allow processing termination*/
   virtual void stop() {}

   /**Notify the node that is will be destroyed shortly*/
   virtual void cleanupNotify() {}

   /**Resets the node internal values and buffers*/
   virtual void reset();

   /**Returns the node name*/
   std::string getName() {return name;}
 
   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      for (unsigned int i=0;i<inputs.size();i++)
         inputs[i].node->request(inputs[i].outputID,req);      
   }

   /**Verify input connections for the node*/
   virtual void verifyConnect();

   /**A node can print itself*/
   void printOn(std::ostream &out=std::cout) const;

   void setUINode(UINode *_uinode) {uinode = _uinode;}
   
   /**Adding a factory into the static dictionary*/
   static int addFactory (const std::string &factoryName, _NodeFactory* const factory);

   /**Adding a XPM representation into the XPM dictionary*/
   static int addXPM (const std::string &nodeName, char **XPMData);

   /**Get the XPM representation from the XPM dictionary*/
   static char**  getXPM (const std::string &nodeName);
   
   /**The factory lookup function*/
   static _NodeFactory* getFactoryNamed (const std::string &name);

protected:

   /**Default constructor, should not be used*/
   Node() {throw new GeneralException("Node Constructor should not be called",__FILE__,__LINE__);}

   /**symbolic to numeric translation for input names*/
   virtual int translateInput(std::string inputName);

   /**symbolic to numeric translation for output names*/
   virtual int translateOutput(std::string inputName);

   /**Run-time assertions*/
   virtual void rt_assert(bool cond, std::string message="", const char *_file="unknown", int _line=0);

   /**Construct-time assertions*/
   virtual void construct_assert(bool cond, std::string message="", const char *_file="unknown", int _line=0);

   /**Error with the node*/
   virtual void throw_error(bool send_ptr, std::string message, const char *_file, int _line);

public:
   /**The node instance factory*/
   static std::map<std::string,_NodeFactory*> &factoryDictionary();

   /**The node information map*/
   static std::vector<std::string> &nodeInfo();

   /**The node visual representation map (XPM)*/
   static std::map<std::string,char**> &XPMDictionary();

   /**Routine to add info for a node*/
   static int addNodeInfo (const std::string &info);
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
   NotInitializedException (std::map<std::string,Node * > aMap) {
      nodeMap = aMap;
   }
   /**The print method*/
   virtual void print(std::ostream &out = std::cerr) {
      out<<"NotInitializedException occured"<<std::endl;
      
      std::map<std::string,Node*>::iterator iter;
      
      for (iter = nodeMap.begin(); iter != nodeMap.end(); iter++) {
         out<<"This node is not initialized: "<<(*iter).first<<std::endl;
      }
   }   

   /**The node map*/
   std::map<std::string,Node*> nodeMap;
};

/** The NodeException is a easy way to send a message for an general 
    exception in a node. You should use __FILE__ and __LINE__ in the 
    arguments.
    @author Jean-Marc Valin
*/
class NodeException : public BaseException {

public:

   /**The constructor with a message a file name and a line number*/
   NodeException( Node *_node, std::string _message, const char *_file, int _line) 
      : message(_message)
      , node(_node)
      , file(_file)
      , line(_line)
      , frozen(false)
   {}   
   /**the print method*/
   virtual void print(std::ostream &out = std::cerr);

   virtual void freeze();

protected:
   /**the message*/
   std::string message;

   /**the node pointer*/
   Node *node;

   /**the file name*/
   std::string file;

   /**the line number*/
   int line;

   /**Whether the exception is frozen*/
   bool frozen;

};



#define DECLARE_NODE_XPM(NodeTypeName, XPMFilePtr) static int dummy_initializer_for ## NodeTypeName = \
               Node::addFactory (# NodeTypeName, new NodeFactory<NodeTypeName>(# NodeTypeName)) + \
               Node::addXPM (# NodeTypeName, XPMFilePtr);


#define DECLARE_NODE(NodeTypeName) DECLARE_NODE_XPM(NodeTypeName,NULL)


}//namespace FD
#endif
