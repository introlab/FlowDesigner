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

#ifndef _NETWORK_CC_
#define _NETWORK_CC_

#include "Network.h"
#include "DLManager.h"


/***************************************************************************/
/*
  initializeFactories()
  Dominic Letourneau
 */
/***************************************************************************/
void Network::initializeFactories() {

   try {
      Node::addFactory ("Constant", new ConstantNodeFactory);
      Node::addFactory ("COLLECTOR", new CollectorNodeFactory);
      Node::addFactory ("MUX", new MuxNodeFactory);
      Node::addFactory ("EXEC", new ExecNodeFactory);
      Node::addFactory ("PATHLIST", new PathListFactory);
      Node::addFactory ("ISVALID", new IsValidFactory);
      Node::addFactory ("SUM", new NodeFactory<Sum>);
      Node::addFactory ("VSUM", new NodeFactory<VSum>);
      Node::addFactory ("VNSUM", new NodeFactory<VNSum>);
      Node::addFactory ("SAVE", new NodeFactory<Save>);
      Node::addFactory ("INPUTSTREAM", new NodeFactory<InputStream>);
      Node::addFactory ("OUTPUTSTREAM", new NodeFactory<OutputStream>);
      Node::addFactory ("SWITCH", new NodeFactory<Switch>);
      Node::addFactory ("NOTDONE", new NodeFactory<NotDone>);
      Node::addFactory ("OR",new NodeFactory<ORNode>);
      Node::addFactory ("AND" , new NodeFactory<ANDNode>);
      Node::addFactory ("NOT", new NodeFactory<NOTNode>);
      Node::addFactory ("LIST", new NodeFactory<List>);
      Node::addFactory ("PACK", new NodeFactory<Pack>);
      Node::addFactory ("UNPACK", new NodeFactory<UnPack>);
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
   : Node(nodeName,params) {

   numNodes = 0;
   sinkNode = NULL;
   inputNode = NULL;
   debugMode = false;
}
/***************************************************************************/
/*
  setDebugMode(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Network::setDebugMode() {

   map<string,Node*>::iterator nodeIter;
   Node *node = NULL;
   
   debugMode = true;
   
   for (nodeIter = nodeDictionary.begin(); nodeIter != nodeDictionary.end(); nodeIter++) {
      node = (*nodeIter).second;
      node->setDebugMode();
   }

}
/***************************************************************************/
/*
  resetDebugMode(...)
  Dominic Letourneau
 */
/***************************************************************************/ 
void Network::resetDebugMode() {
  
   map<string,Node*>::iterator nodeIter;
   Node *node = NULL;
   
   debugMode = false;
   
   for (nodeIter = nodeDictionary.begin(); nodeIter != nodeDictionary.end(); nodeIter++) {
      node = (*nodeIter).second;
      node->resetDebugMode();
   }
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
 

   /*while (factoryDictionary.size() > 0) {

      factoryIter = factoryDictionary.begin();
      if (debugMode) {
         cout<<"Deleting factory : "<<(*factoryIter).first<<endl;
      }
      factory = (*factoryIter).second;
      factoryDictionary.erase((*factoryIter).first);  
      delete factory;
      }*/

   //deleting all nodes in the dictionary
   Node* node = NULL;
   map<string,Node*>::iterator nodeIter;
   
   while (nodeDictionary.size() > 0)  {
      nodeIter = nodeDictionary.begin();
      if (debugMode) {
         cout<<"Deleting node : "<<(*nodeIter).first<<endl;
      }
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
  void tryPluginNode(string name)
  Jean-Marc Valin
 */
/***************************************************************************/
Node *tryPluginNode(const string &name, const string &nodeName, const ParameterSet &parameters)
{
   /*cerr << "Trying to load node " << name << " dynamically" << endl;
   cerr << "Not supported" << endl;
   void *handle = dlopen (name.c_str(), RTLD_LAZY);
   cerr << "handle = " << handle << endl;
   void *sym = dlsym (handle, "createNewNode");
   cerr << "sym = " << sym << endl;*/
   LoadedLibrary *library = (LoadedLibrary *) DLManager::get_lib(name);
   void *sym = library->get_proc("createNewNode");
   Node *(*new_funct)(string, ParameterSet) = (Node *(*)(string, ParameterSet)) (sym);

   return new_funct(nodeName,parameters);
   //return NULL;
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
      node = tryPluginNode(factoryName, nodeName, parameters);
      //cerr << "node = " << node << endl;
      if (!node)
         throw FactoryNotFoundException(factoryName);
   } else {
      //creating an instance of the specified node.
      node = factory->Create(nodeName, parameters);
   }

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
      throw NodeNotFoundException(nodeName);
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
   
   if (currentNode && inputNode) {

      if (debugMode) {
         cout<<"Network "<<name<<" : Connecting "<<currentNodeName<<" ["<<inputName<<"]"
             <<" to "<<inputNodeName<<" ["<<outputName<<"]"<<endl;
      }

     currentNode->connectToNode(inputName,inputNode,outputName);
   }
   else {
      if (!currentNode) {throw NodeNotFoundException(currentNodeName);}
      if (!inputNode) {throw NodeNotFoundException(inputNodeName);}
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
      throw NoSinkNodeException();
   }

   Node* node = NULL;
   map<string,Node*>::iterator iter;
   map<string,Node*> connectionMap;
   
  
   sinkNode->initialize();
   
   //we must verify if all the nodes are initialized properly

   for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) {
      if (!((*iter).second)->isInitialized()) {
         //adding the nodes that are not properly initialized
         connectionMap.insert(nodeEntry((*iter).first, (*iter).second));
      }
   }

   if (connectionMap.size() > 0) {
      throw NotInitializedException(connectionMap);
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
      throw NoSinkNodeException();
   }
   lock();
   return sinkNode->getOutput(output_id, count);
   unlock();
}

/***************************************************************************/
/*
  Network::hasOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
bool Network::hasOutput (int output_id) const {

   if (!sinkNode) {
      throw NoSinkNodeException();
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
      throw NoInputNodeException();
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
      throw NoSinkNodeException();
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
   if (!inputNode) {
      throw NoInputNodeException();
   }
   inputNode->connectToNode(in,inNode,out);
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
      if (debugMode) {
         cout<<"DEBUG : Network is now resetting node "<<(*iter).second->getName()<<endl;
      }
      (*iter).second->reset();
   }
}
#endif
