// Copyright (C) 1999 Jean-Marc Valin, Dominic Letourneau and Andre Charbonneau
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
//#include "NodeHeaders.h"
#include "Exception.h"
#include "Node.h"


/**
   The Pull Network Class. It holds the nodes which are connected by the Network.
   A network has always a single input node and a sink node (output node).
   We use a Factory for the cration of Node instances. That way, we can use a 
   lookup dictionary that contains a name for that factory and a pointer to a
   Factory Base class that uses a virtual function callde Create(). The initialization must
   begin at the sink node and is propagated through the network until it reaches
   the input node.A network can be included as a node as well.
 */

//@author Dominic Letourneau
//@version 1.0

class Network : public Node {

public:


   /**Subnet: NetworkNode constructor*/
   Network (string nodeName, ParameterSet params);

   /**default destructor*/
   ~Network ();
   

   /**
      Returns the associated Node pointer from a lookup dictionary with the node name.
      We return NULL if the node doesn't exist.
   */
   Node* getNodeNamed (const string &name);
   
   /** Adding a node to the network. We use a factory Name to create an instance
       of the Node and store the node in the node dictionary with an associated name.*/
   void addNode (const string &factoryName,const string &nodeName, const ParameterSet &parameters);
  
   /** Adding a already constructed node. WATCHOUT , THE NODE IS NOT COPIED AT THE MOMENT */
   void addNode (Node &node);

   /**connecting two nodes. We are using the node names.*/
   virtual void connect (const string &currentNodeName,const string &inputName, 
                 const string &inputNodeName, const string &outputName);
 
   /**removing a node. We are using the node name.*/
   Node* removeNode (const string &nodeName);
   
   /**Returns the name of the node (the same as in the node dictionary)*/
   string getName() {return name;}

   /**Naming the current network*/
   void setName(const string &my_name) {name = my_name;}

   /**Returns the sinkNode*/
   Node* getSinkNode () {return sinkNode;}

   /**Setting the sink node (unique)*/
   virtual void setSinkNode (Node* node) {sinkNode = node;} 
 
   /**Returns the inputNode pointer*/
   Node* getInputNode () {return inputNode;}

   /**Setting the input node (unique)*/
   virtual void setInputNode (Node* node) {inputNode = node;}

   /**Returns the inputs vector */
   virtual vector<NodeInput>& getInputs () {
      if (!inputNode) throw NodeException(this,"No inputNode",__FILE__,__LINE__);
      return inputNode->getInputs();
   }

   /**Setting the debug mode*/
   virtual void setDebugMode();

   /**Exiting debug mode*/
   virtual void resetDebugMode();

   /** resets the Network and all the internal nodes */
   virtual void reset();
   
   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req) {sinkNode->request(outputID,req);}

   /**Subnet : NetworkNode specific initialize*/
   virtual void specificInitialize();

   /**Subnet : NetworkNode returns the output of the SubNet (from the sinkNode)*/
   virtual ObjectRef getOutput (int output_id, int count);

   /**Subnet : checks if the sinkNode has the desired output*/
   virtual bool hasOutput (int output_id) const;

   /**Subnet : The connectToNode method overloaded from Node */
   virtual void connectToNode(string in, Node *inNode, string out) {
      if (!inputNode) {
         throw NodeException(this,string("No input node in iterator :") + name, __FILE__,__LINE__);
      }
      connectToNode(inputNode->translateInput(in), inNode, inNode->translateOutput(out));      
   }
   

protected: 

   
   /**Subnet : getting the related number of the input description*/
   virtual int translateInput (string   inputName);

   /**Subnet : getting the related number of the output description*/
   virtual int translateOutput (string outputName);
  
   /**Subnet : Connect an input node using numeric (integer) input/output names*/
   virtual void connectToNode(unsigned int in, Node *inNode, unsigned int out);

   /**The number of nodes in the network*/
   int numNodes;

   /**The node dictionary*/
   map<string,Node*> nodeDictionary;
   /**The sink node*/
   Node *sinkNode;
   /**The input node*/
   Node *inputNode;
   /**The debug mode flag*/
   bool debugMode;
 



   
   /**default constructor should never be used*/
   Network () {
     throw NodeException (NULL,"The default constructor should not be called from Network",__FILE__,__LINE__);
   } 
   
};
#endif
