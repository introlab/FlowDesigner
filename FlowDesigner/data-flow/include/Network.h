#ifndef _NETWORK_H_
#define _NETWORK_H_

#include <map>
#include <string>
#include "NodeFactory.h"
#include "NodeHeaders.h"
#include "NetworkException.h"

typedef map<string, Node*>::value_type nodeEntry;
typedef map<string, _NodeFactory*>::value_type factoryEntry; 



class Network {

public:

   Network (); 
   ~Network ();
   
   Node* getNodeNamed (const string &name);
   void addNode (const string &factoryName,const string &nodeName, const ParameterSet &parameters);
   void connect (const string &currentNodeName,const string &inputName, 
                 const string &inputNodeName, const string &outputName);
   Node* removeNode (const string &nodeName);


   string getName() {return netName;}
   void setName(const string &name) {netName = name;}
   Node* getSinkNode () {return sinkNode;}
   void setSinkNode (Node* node) {sinkNode = node;}  
   Node* getInputNode () {return inputNode;}
   void setInputNode (Node* node) {inputNode = node;}
   bool isDebugMode () {return debugMode;}
   void setDebugMode() {debugMode = 1;}
   void resetDebugMode() {debugMode = 0;}
   void initialize ();

private:

   int numNodes;
   map<string,_NodeFactory*> factoryDictionary;
   map<string,Node*> nodeDictionary;
   string netName;
   Node *sinkNode;
   Node *inputNode;
   bool debugMode;
   _NodeFactory* getFactoryNamed (const string &name);

};
#endif
