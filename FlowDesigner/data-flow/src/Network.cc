// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "Network.h"

//@implements core

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
   //Propagate the cleanupNotify request
   map<string,Node*>::iterator nodeIter;
   for (nodeIter = nodeDictionary.begin(); nodeIter != nodeDictionary.end(); nodeIter++)
   {
      Node *node = (*nodeIter).second;
      node->cleanupNotify();
   }
}

/***************************************************************************/
/*
  stop(...)
  Jean-Marc Valin
 */
/***************************************************************************/
void Network::stop()
{
   //Propagate the stop request
   map<string,Node*>::iterator nodeIter;
   for (nodeIter = nodeDictionary.begin(); nodeIter != nodeDictionary.end(); nodeIter++)
   {
      Node *node = (*nodeIter).second;
      node->stop();
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
  Network::initialize(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::initialize() {

   this->Node::initialize();
   
   //We need a sink Node
   if (!sinkNode) {
     throw new NoSinkNodeException();
   }
   
   //We must call initialize to all our nodes
   for (map<string,Node*>::iterator iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) {
     try {
        (*iter).second->initialize();
     } catch (BaseException *e)
     {
        throw e->add(new GeneralException(string("Exception caught while initializing ") + (*iter).second->getName(), __FILE__, __LINE__));
     }
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
      throw new NodeException(this,string("No input node in subnet :") + name, __FILE__,__LINE__);
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
