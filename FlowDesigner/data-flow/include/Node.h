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
   NodeInput() {outputID=-1; } //-1 means unused
private:
   
};

typedef map<string,ObjectRef> ParameterSet;
typedef map<string,ObjectRef>::value_type ParameterEntry;

class Node { 
protected:
   string name;
   vector<NodeInput> inputs;
   bool initialized;
   bool debugMode;
   int processCount;
   int outputInitializeCount;
   long status;
   map <string, ObjectRef> parameters;
public:
   Node(string nodeName, ParameterSet params);   //default constructor
   //Node(const Node& node); //copy constructor
   virtual ~Node() {}         //default destructor
   virtual ObjectRef getOutput(int output_id, int count)  = 0; 
   virtual void setParameter(string paramName, Object *value) {};
   void connectToNode(string in, Node *inputNode, string out);
   void connectToNode(unsigned int in, Node *inputNode, unsigned int out);
   void initialize ();
   virtual void specificInitialize();
   virtual bool hasOutput(int output_id) const = 0;
   bool isInitialized() {return initialized;}
   bool isDebugMode() {return debugMode;}
   void setDebugMode(){debugMode = 1;}
   void resetDebugMode(){debugMode = 0;}
   void reset();
private:
   void registerOutput (int out) {outputInitializeCount++;}
   void incrementOutputInitialize() {outputInitializeCount++;}
   virtual int translateInput(string inputName) = 0;
   virtual int translateOutput(string inputName) = 0;
protected:
   Node() {throw "Node error";};
};




#endif
