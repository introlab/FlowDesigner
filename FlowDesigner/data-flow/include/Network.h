// Copyright (C) 1998-1999 Jean-Marc Valin,
// Dominic Letourneau and Andre Charbonneau
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

#ifndef _NETWORK_H_
#define _NETWORK_H_

#include <map>
#include <string>
#include "NodeFactory.h"
#include "NodeHeaders.h"
#include "NetworkException.h"

///Definition of the type we need for the dictionaries
typedef map<string, Node*>::value_type nodeEntry;
typedef map<string, _NodeFactory*>::value_type factoryEntry; 

/**
   The Pull Network Class. It holds the nodes which are connected by the Network.
   A network has always a single input node and a sink node (output node).
   We use a Factory for the cration of Node instances. That way, we can use a 
   lookup dictionary that contains a name for that factory and a pointer to a
   Factory Base class that uses a virtual function callde Create(). The initialization must
   begin at the sink node and is propagated through the network until it reaches
   the input node.
 */
class Network {

public:

   ///default constructor
   Network (); 

   ///default destructor
   ~Network ();
   
   /**
      Returns the associated Node pointer from a lookup dictionary with the node name.
      We return NULL if the node doesn't exist.
   */
   Node* getNodeNamed (const string &name);
   
   /** Adding a node to the network. We use a factory Name to create an instance
       of the Node and store the node in the node dictionary with an associated name.*/
   void addNode (const string &factoryName,const string &nodeName, const ParameterSet &parameters);
  
   ///connecting two nodes. We are using the node names.
   void connect (const string &currentNodeName,const string &inputName, 
                 const string &inputNodeName, const string &outputName);
 
   ///removing a node. We are using the node name.
   Node* removeNode (const string &nodeName);
   
   ///Returns the name of the node (the same as in the node dictionary)
   string getName() {return netName;}

   ///Naming the current network
   void setName(const string &name) {netName = name;}
   Node* getSinkNode () {return sinkNode;}

   ///Setting the sink node (unique)
   void setSinkNode (Node* node) {sinkNode = node;} 
 
   ///Returns the inputNode pointer
   Node* getInputNode () {return inputNode;}

   ///Setting the input node (unique)
   void setInputNode (Node* node) {inputNode = node;}

   ///If we want to know if we are in debug mode.
   bool isDebugMode () {return debugMode;}

   ///Setting the debug mode
   void setDebugMode() {debugMode = 1;}

   ///Exiting debug mode
   void resetDebugMode() {debugMode = 0;}

   /** 
       Network initialization. Must be done after all connections.
       The sink node must be set.
   */
   void initialize (); 
   
   ///Adding a factory into the static dictionary
   static void addFactory (const string &factoryName, _NodeFactory* const factory);


private:

   ///The number of nodes in the network
   int numNodes;
   ///The node instance factory
   static map<string,_NodeFactory*> factoryDictionary;
   ///The node dictionary
   map<string,Node*> nodeDictionary;
   ///The network name
   string netName;
   ///The sink node
   Node *sinkNode;
   ///The input node
   Node *inputNode;
   ///The debug mode flag
   bool debugMode;
   ///The factory lookup function
   static _NodeFactory* getFactoryNamed (const string &name);

};
#endif
