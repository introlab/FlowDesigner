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
#include "NetworkException.h"
#include <typeinfo>

class Node;

class NodeInput {
public:
   int outputID;
   Node *node;
   NodeInput(Node *n, int t) :outputID(t),node(n) {}
   NodeInput() : outputID(-1) {} //-1 means unused
private:
   
};


class ParameterSet : public map<string,ObjectRef> {
public: 
   void requireParam(string param);
   ObjectRef get(string param);
   ObjectRef getDefault(string param, ObjectRef value);
   void defaultParam(string param, ObjectRef value);
   void add(string param, ObjectRef value);
   void print(ostream &out = cerr);
};


class MissingParameterException : public NetworkBaseException {

public:
   MissingParameterException(string _param_name, ParameterSet _params) 
      : param_name(_param_name)
      , params(_params)
   {}   

   virtual void print(ostream &out = cerr) 
   {
      out<<"Missing parameter: "<< param_name <<endl;
      out << "Given parameters are:\n";
      params.print(out);
   }
protected:
   string param_name;
   ParameterSet params;
};


typedef map<string,ObjectRef>::value_type ParameterEntry;

///Base node class

class Node { 

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
   Node(string nodeName, ParameterSet params);
   
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

private:
   ///Tell the node we will be using output 'out'
   void registerOutput (int out) {outputInitializeCount++;}

   ///Increment outputInitializeCount when performing a reset()
   void incrementOutputInitialize() {outputInitializeCount++;}


protected:

   ///Default constructor, should not be used
   Node() {throw new GeneralException("Node Constructor should not be called",__FILE__,__LINE__);}

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
class NotInitializedException : public NetworkBaseException {

public:
   NotInitializedException (map<string,Node*> aMap) {
      nodeMap = aMap;
   }
   
   virtual void print(ostream &out = cerr) {
      out<<"NotInitializedException occured"<<endl;
      
      map<string,Node*>::iterator iter;
      
      for (iter = nodeMap.begin(); iter != nodeMap.end(); iter++) {
         out<<"This node is not initialized: "<<(*iter).first<<endl;
      }
   }   

   //variables
   map<string,Node*> nodeMap;
};


class NodeException : public NetworkBaseException {

public:
   NodeException( Node *_node, string _message, char *_file, int _line) 
      : message(_message)
      , node(_node)
      , file(_file)
      , line(_line)
   {}   

   virtual void print(ostream &out = cerr) 
   {
      if (node)
         out << file << " line " << line << ": Node " << node->getName() 
             << " (type " << typeid(*node).name() << ") " << message << endl;
      else out << file << ", line " << line << ": " << message << endl;
   }
protected:
   string message;
   Node *node;
   string file;
   int line;
};

#endif
