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


#include "Network.h"
//#include "DLManager.h"

/***************************************************************************/
/*
  Network(...)
  Dominic Letourneau
 */
/***************************************************************************/
Network::Network (string nodeName, ParameterSet params) 
   : Node(nodeName,params) {

   numNodes = 0;
   sinkNode = NULL;
   inputNode = NULL;
}


/***************************************************************************/
/*
  cleanupNotify(...)
  Jean-Marc Valin
 */
/***************************************************************************/
void Network::cleanupNotify()
{
   map<string,Node*>::iterator nodeIter;
   
   for (nodeIter = nodeDictionary.begin(); nodeIter != nodeDictionary.end(); nodeIter++)
   {
      Node *node = (*nodeIter).second;
      node->cleanupNotify();
   }

}




/***************************************************************************/
/*
  ~Network()
  Dominic Letourneau
 */
/***************************************************************************/
Network::~Network() {


   //deleting all nodes in the dictionary
   Node* node = NULL;
   map<string,Node*>::iterator nodeIter;
   
   while (nodeDictionary.size() > 0)  {
      nodeIter = nodeDictionary.begin();

      node = (*nodeIter).second;
      nodeDictionary.erase((*nodeIter).first);
      delete node;
   }
}


/***************************************************************************/
/*
  Network::getNodeNamed (...)
  Dominic Letourneau
 */
/***************************************************************************/
Node* Network::getNodeNamed (const string &name){

   Node* node = NULL;
   map<string,Node*>::iterator iter;
   
   //let's find the key in our map
   //if not found will return NULL
   for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) {
      if ((*iter).first == name) {
         node = (*iter).second;
         break;
      }
   }
   return node;
}


/***************************************************************************/
/*
  Network::addNode (...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::addNode (const string &factoryName,const string &nodeName, const ParameterSet &parameters) {
   _NodeFactory *factory = NULL;
   Node *node = NULL;

   factory = getFactoryNamed(factoryName);
   if (!factory)
      throw new FactoryNotFoundException(factoryName);
      
   //creating an instance of the specified node.
   node = factory->Create(nodeName, parameters);

   //inserting in the node dictionary
   //maybe we should look for duplicate entries...
   nodeDictionary.insert (nodeEntry (nodeName,node));

   numNodes++;
   //cout<<"Node inserted: "<< nodeName <<" node pointer :"<<node<<endl;

}


/***************************************************************************/
/*
  Network::addNode (...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::addNode (Node &node) {
   
   nodeDictionary.insert(nodeEntry (node.getName(),&node));
   numNodes++;
}


/***************************************************************************/
/*
  Network::removeNode (...)
  Dominic Letourneau
 */
/***************************************************************************/
Node * Network::removeNode (const string &nodeName) {

   Node* node = getNodeNamed(nodeName);
   
   if (node) {
      //removing from dictionary
      nodeDictionary.erase(nodeName);
      numNodes--;
   }
   else {
      throw new NodeNotFoundException(nodeName);
   }

   return node;
}


/***************************************************************************/
/*
  Network::connect (...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::connect (const string &currentNodeName,const string &inputName, 
                       const string &inputNodeName, const string &outputName) {

   Node* currentNode = getNodeNamed(currentNodeName);
   Node* inputNode = getNodeNamed(inputNodeName);
   
   if (currentNode && inputNode) 
   {
     currentNode->connectToNode(inputName,inputNode,outputName);
   }
   else {
      if (!currentNode) {throw new NodeNotFoundException(currentNodeName);}
      if (!inputNode) {throw new NodeNotFoundException(inputNodeName);}
   }
   

}


/***************************************************************************/
/*
  verifyConnect(...)
  Jean-Marc Valin
 */
/***************************************************************************/
void Network::verifyConnect()
{
   map<string,Node*>::iterator iter;

   for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) 
   {
      (*iter).second->verifyConnect();
   }
}


/***************************************************************************/
/*
  Network::specificInitialize(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::specificInitialize() {
   this->Node::specificInitialize();
   
   //We need a sink Node
   if (!sinkNode) {
      throw new NoSinkNodeException();
   }
   
   map<string,Node*>::iterator iter;


   map<string,Node*> connectionMap;
   
  
   sinkNode->initialize();
   
   //we must verify if all the nodes are initialized properly

   for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) 
   {
      if (!((*iter).second)->isInitialized()) 
      {
         //adding the nodes that are not properly initialized
         connectionMap.insert(nodeEntry((*iter).first, (*iter).second));
      }
   }

   if (connectionMap.size() > 0) {
      throw new NotInitializedException(connectionMap);
   }


}

/***************************************************************************/
/*
  Network::getOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
ObjectRef Network::getOutput (int output_id, int count) {

   if (!sinkNode) {
      throw new NoSinkNodeException();
   }
   return sinkNode->getOutput(output_id, count);
}

/***************************************************************************/
/*
  Network::hasOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
bool Network::hasOutput (int output_id) const {

   if (!sinkNode) {
      throw new NoSinkNodeException();
   }
   return sinkNode->hasOutput(output_id);
}

/***************************************************************************/
/*
  Network::translateInput(...)
  Dominic Letourneau
 */
/***************************************************************************/
int Network::translateInput (string   inputName) {
   if (!inputNode) {
      throw new NoInputNodeException();
   }
   return inputNode->translateInput (inputName);
}

/***************************************************************************/
/*
  Network::translateOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
int Network::translateOutput (string outputName) {

   if (!sinkNode) {
      throw new NoSinkNodeException();
   }
   return sinkNode->translateOutput(outputName);
}

/***************************************************************************/
/*
  Network::connectToNode(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::connectToNode(unsigned int in, Node *inNode, unsigned int out) {
   if (!inputNode)
      throw new NoInputNodeException();
   
   inputNode->connectToNode(in,inNode,out);
}

/**Subnet : The connectToNode method overloaded from Node */
void Network::connectToNode(string in, Node *inNode, string out) 
{
   if (!inputNode) 
   {
      throw new NodeException(this,string("No input node in iterator :") + name, __FILE__,__LINE__);
   }
   connectToNode(inputNode->translateInput(in), inNode, inNode->translateOutput(out));      
}

/***************************************************************************/
/*
  Network::reset(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::reset() {

    map<string,Node*>::iterator iter;   

   for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) {
      (*iter).second->reset();
   }
}
