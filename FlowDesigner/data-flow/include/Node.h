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
#include "Object.h"
//#include "net_types.h"


class Node;

class NodeInput {
public:
   int outputID;
   Node *node;
   NodeInput(Node *n, int t) :outputID(t),node(n) {}
   NodeInput() : outputID(-1) {} //-1 means unused
private:
   
};

typedef map<string,ObjectRef> ParameterSet;
typedef map<string,ObjectRef>::value_type ParameterEntry;

///Base node class
class Node { 
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
   map <string, ObjectRef> parameters;

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

private:
   ///Tell the node we will be using output 'out'
   void registerOutput (int out) {outputInitializeCount++;}

   ///Increment outputInitializeCount when performing a reset()
   void incrementOutputInitialize() {outputInitializeCount++;}

   ///symbolic to numeric translation for input names
   virtual int translateInput(string inputName) = 0;

   ///symbolic to numeric translation for output names
   virtual int translateOutput(string inputName) = 0;
protected:
   ///Default constructor, should not be used
   Node() {throw "Node error";};
};




#endif
