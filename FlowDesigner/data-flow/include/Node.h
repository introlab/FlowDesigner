// Copyright (C) 1998-1999 Jean-Marc Valin & Dominic Letourneau
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
#include "ObjectRef.h"
//#include "net_types.h"
#include "Exception.h"
#include <typeinfo>


template <class T>
T &max(T &a, T &b) {return a > b ? a : b;}

template <class T>
T &min(T &a, T &b) {return a < b ? a : b;}


//must be defined
class Node;

/** A NodeInput is a data structure that holds a reference to
    the node we are connected at and its related output number.
    @author Jean-Marc Valin
    @version 1.0
*/
class NodeInput {
public:
   ///The outputID of the connected node
   int outputID;
   ///The reference of the node
   Node *node;
   ///Constructor with a node and an outputID
   NodeInput(Node *n, int t) :outputID(t),node(n) {}
   ///Copy constructor
   NodeInput (const NodeInput &in) {
      node = in.node; outputID = in.outputID;
   }
   ///equality operator
   NodeInput& operator= (const NodeInput &in) {
      node = in.node; outputID = in.outputID; return *this;
   }
   ///default constructor
   NodeInput() : outputID(-1) {} //-1 means unused
private:   
};

/** A ParameterSet is a data structure that holds all the parameters 
    needed for the construction of a new node.
    @author Jean-Marc Valin
    @version 1.0
*/
class ParameterSet : public map<string,pair<ObjectRef,bool> > {
public:
   ///Does a certain parameter exist?
   bool exist(const string &param) const;

   ///get a parameter's value
   ObjectRef get(string param) const;

   ///get the default parameter
   ObjectRef getDefault(string param, ObjectRef value);

   ///set the default parameter
   void defaultParam(string param, ObjectRef value);

   ///adding the parameters
   void add(string param, ObjectRef value);

   ///printing the parameters
   void print(ostream &out = cerr) const;
};

/** The MissingParameterException occurs when a node needs a parameter
    for its initialization and couldn't find it. 
    @author Jean-Marc Valin
    @version 1.0
*/
class MissingParameterException : public BaseException {

public:
   ///The constructor with the parameters
   MissingParameterException(string _param_name, ParameterSet _params) 
      : param_name(_param_name)
      , params(_params)
   {}   
   ///The print method
   virtual void print(ostream &out = cerr) 
   {
      out<<"Missing parameter: "<< param_name <<endl;
      out << "Given parameters are:\n";
      params.print(out);
   }
protected:
   ///the parameter name
   string param_name;
   ///the parameter set
   ParameterSet params;
};

///A parameter entry in the parameterSet
typedef map<string,ObjectRef>::value_type ParameterEntry;

/* The Base Node class. All nodes to be inserted in a network must
   derive from this class. It contains the proper initializations
   for the connectivity of the nodes.
   @author Jean-Marc Valin & Dominic Letourneau
   @version 1.0
*/
class Node { 

   ///A network can have access to private members of Node
   friend class Network;

protected:
   ///Node's name
   string name;

   ///Node's inputs
   vector<NodeInput> inputs;

   ///Whether the node has been initialized
   bool initialized;
   
   ///Is the node in debug Mode
   bool debugMode;

   /**Internal processing counter for synchronization.
      This counter is used to find out whether the output of the node
      needs to be updated */
   int processCount;
   
   /**Used during initialization.
      Becomes zero when all the node's outputs have been initialized*/
   int outputInitializeCount;
   
   ///Node's status
   long status;
   
   ///Parameters given to the node at construction time
   ParameterSet parameters;

public:
   ///Constructor, takes the name of the node and a set of parameters
   Node(string nodeName, const ParameterSet &params);
   
   //Node(const Node& node); //copy constructor

   ///Destructor
   virtual ~Node() {}

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)  = 0; 

   ///Connect an input node using symbolic (strings) input/output names
   void connectToNode(string in, Node *inputNode, string out);

   ///Connect an input node using numeric (integer) input/output names
   void connectToNode(unsigned int in, Node *inputNode, unsigned int out);

   ///Initialize a node
   void initialize ();

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   ///Checks whether node really has a certain output
   virtual bool hasOutput(int output_id) const = 0;

   ///Has the node been initialized?
   bool isInitialized() {return initialized;}

   ///Is the node in debug mode?
   bool isDebugMode() {return debugMode;}

   ///Sets the node to debug mode
   void setDebugMode(){debugMode = 1;}

   ///Resets debug mode
   void resetDebugMode(){debugMode = 0;}

   ///Resets the node internal values and buffers
   void reset();

   ///Returns the node name
   string getName() {return name;}   
 
   ///Standard request-passing method between nodes during initialization
   virtual void request(const ParameterSet &req) {}

private:
   ///Tell the node we will be using output 'out'
   void registerOutput (int out) {outputInitializeCount++;}

   ///Increment outputInitializeCount when performing a reset()
   void incrementOutputInitialize() {outputInitializeCount++;}


protected:
   ///Default constructor, should not be used
   Node() {throw GeneralException("Node Constructor should not be called",__FILE__,__LINE__);}

   ///symbolic to numeric translation for input names
   virtual int translateInput(string inputName) = 0;

   ///symbolic to numeric translation for output names
   virtual int translateOutput(string inputName) = 0;
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
   ///The constructor that takes a map of nodes not properly initialized
   NotInitializedException (map<string,Node*> aMap) {
      nodeMap = aMap;
   }
   ///The print method
   virtual void print(ostream &out = cerr) {
      out<<"NotInitializedException occured"<<endl;
      
      map<string,Node*>::iterator iter;
      
      for (iter = nodeMap.begin(); iter != nodeMap.end(); iter++) {
         out<<"This node is not initialized: "<<(*iter).first<<endl;
      }
   }   

   ///The node map
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

   ///The constructor with a message a file name and a line number
   NodeException( Node *_node, string _message, char *_file, int _line) 
      : message(_message)
      , node(_node)
      , file(_file)
      , line(_line)
   {}   
   ///the print method
   virtual void print(ostream &out = cerr) 
   {
      if (node)
         out << file << " line " << line << ": Node " << node->getName() 
             << " (type " << typeid(*node).name() << ") " << message << endl;
      else out << file << ", line " << line << ": " << message << endl;
   }
protected:
   ///the message
   string message;
   ///the node pointer
   Node *node;
   ///the file name
   string file;
   ///the line number
   int line;
};

inline bool ParameterSet::exist(const string &param) const
{
   if (find(param)!=end())
   {
      (const_cast <ParameterSet *> (this))->find(param)->second.second = true;
      return true;
   }
   return false;
}

inline ObjectRef ParameterSet::get(string param) const
{
   if (find(param)==end())
      throw MissingParameterException(param,*this);
   //else return operator[](param);
   else 
      {
         (const_cast <ParameterSet *> (this))->find(param)->second.second = true;
         return find(param)->second.first;
      }
}
inline ObjectRef ParameterSet::getDefault(string param, ObjectRef value) 
{
   if (find(param)==end()) 
      return value;
   else return operator[](param).first;
}
inline void ParameterSet::defaultParam(string param, ObjectRef value)
{
   if (find(param)==end())
      (operator[](param))=pair<ObjectRef,bool> (value,false);
}
inline void ParameterSet::add(string param, ObjectRef value)
{
   (operator[](param))=pair<ObjectRef,bool> (value,false);
}

inline void ParameterSet::print (ostream &out) const
{
   for (ParameterSet::const_iterator it=begin(); it!=end();it++)
      out << it->first << " -> " << typeid(*(it->second.first)).name() << endl;
}

#endif
