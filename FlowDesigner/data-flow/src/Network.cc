#ifndef _NETWORK_CC_
#define _NETWORK_CC_

#include "Network.h"

//our static factory dictionary
map<string,_NodeFactory*> Network::factoryDictionary;

/***************************************************************************/
/*
  initializeFactories()
  Dominic Letourneau
 */
/***************************************************************************/
void Network::initializeFactories() {

   try {
      Network::addFactory ("CONST", new ConstantNodeFactory);
   }
   catch (...) {
      cerr<<"Factories already initialized..."<<endl;
   }
}
/***************************************************************************/
/*
  Network(...)
  Dominic Letourneau
 */
/***************************************************************************/
Network::Network (string nodeName, ParameterSet params) 
   : netName(nodeName), Node(nodeName,params) {

   numNodes = 0;
   sinkNode = NULL;
   inputNode = NULL;
   debugMode = false;
}
/***************************************************************************/
/*
  ~Network()
  Dominic Letourneau
 */
/***************************************************************************/
Network::~Network() {

   //deleting all factory in the dictionary
   _NodeFactory* factory = NULL;
   map<string, _NodeFactory*>::iterator factoryIter;
 

   while (factoryDictionary.size() > 0) {

      factoryIter = factoryDictionary.begin();
      cout<<"Deleting factory : "<<(*factoryIter).first<<endl;
      factory = (*factoryIter).second;
      factoryDictionary.erase((*factoryIter).first);  
      delete factory;
   }

   //deleting all nodes in the dictionary
   Node* node = NULL;
   map<string,Node*>::iterator nodeIter;
   
   while (nodeDictionary.size() > 0)  {
      nodeIter = nodeDictionary.begin();
      cout<<"Deleting node : "<<(*nodeIter).first<<endl;
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
   if (!factory) {
      throw ExceptionRef (new FactoryNotFoundException(factoryName));
   }

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
      throw ExceptionRef (new NodeNotFoundException(nodeName));
   }

   return node;
}
/***************************************************************************/
/*
  Network::getFactoryNamed (...)
  Dominic Letourneau
 */
/***************************************************************************/
_NodeFactory* Network::getFactoryNamed (const string &name) {

   _NodeFactory* factory = NULL;
   map<string,_NodeFactory*>::iterator iter;
   
   //let's find the key in our map
   //if not found will return NULL
   for (iter = factoryDictionary.begin(); iter != factoryDictionary.end(); iter++) {
      if ((*iter).first == name) {
         factory = (*iter).second;
         break;
      }
   }
   return factory;
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
   
   if (currentNode && inputNode) {
      currentNode->connectToNode(inputName,inputNode,outputName);
   }
   else {
      if (!currentNode) {throw ExceptionRef (new NodeNotFoundException(currentNodeName));}
      if (!inputNode) {throw ExceptionRef (new NodeNotFoundException(inputNodeName));}
   }
   

}
/***************************************************************************/
/*
  Network::initialize()
  Dominic Letourneau
 */
/***************************************************************************/
void Network::initialize() {

   Node* node = NULL;
   map<string,Node*>::iterator iter;
   map<string,Node*> connectionMap;
   

   if (sinkNode) {
      sinkNode->initialize();
   }
   else {
      throw ExceptionRef (new NoSinkNodeException());
   }

   //we must verify if all the nodes are initialized properly

   for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) {
      if (!((*iter).second)->isInitialized()) {
         //adding the nodes that are not properly initialized
         connectionMap.insert(nodeEntry((*iter).first, (*iter).second));
      }
   }

   if (connectionMap.size() > 0) {
      throw ExceptionRef (new NotInitializedException(connectionMap));
   }
}
/***************************************************************************/
/*
  Network::addFactory()
  Dominic Letourneau
 */
/***************************************************************************/
void Network::addFactory (const string &factoryName, _NodeFactory* const factory) {
   if (!getFactoryNamed(factoryName)) {
      //the factory doesn't exist inserting it...
      if (factory != NULL) {
         factoryDictionary.insert (factoryEntry(factoryName,factory));
      }
      else {
         cerr<<"NULL _NodeFactory pointer, exiting"<<endl;
         exit(-1);
      }
   }
   else {
      throw ExceptionRef (new NodeException (NULL,"The factory already exists",__FILE__,__LINE__));
   }
};
/***************************************************************************/
/*
  Network::specificInitialize(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::specificInitialize() {
   this->Node::specificInitialize();



   //We need an input Node
   if (!inputNode) {
      throw ExceptionRef (new NoInputNodeException());
   }
   
   //We need a sink Node
   if (!sinkNode) {
      throw ExceptionRef (new NoSinkNodeException());
   }

   /* We must be careful with the input node. 
      If we are a subnet we must copy all the Network inputs to the
      InputNode inputs  if we want the requests to be propagated.
   */

   vector<NodeInput>::iterator in;

   //resizing
   inputNode->inputs.resize(inputs.size());
   
   //copying
   inputNode->inputs = inputs;

}

/***************************************************************************/
/*
  Network::getOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
ObjectRef Network::getOutput (int output_id, int count) {

   if (!sinkNode) {
      throw ExceptionRef (new NoSinkNodeException());
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
      throw ExceptionRef (new NoSinkNodeException());
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
      throw ExceptionRef (new NoInputNodeException());
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
      throw ExceptionRef (new NoSinkNodeException());
   }
   return sinkNode->translateOutput(outputName);
}
#endif
