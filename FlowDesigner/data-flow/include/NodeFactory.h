// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef _NODEFACTORY_H_
#define _NODEFACTORY_H_


/*
  author: Dominic Letourneau
  date  : 09/06/99 
 */

//#include "Node.h"
#include "ParameterSet.h"
#include <vector>
#include <string>

class Node;

//abstract factory class
class _NodeFactory {
protected:
   std::string name;
   
public:
   _NodeFactory(std::string _name)
      : name(_name)
   {}   
   virtual const std::string &getName() {return name;}

   virtual Node* Create(const std::string &name, const ParameterSet &parameters) = 0;
   virtual ~_NodeFactory() {;}

private:


};


//Template class used by all Nodes
template <class T>
class NodeFactory : public _NodeFactory {
public:
   NodeFactory(std::string _name)
      : _NodeFactory(_name)
   {}
   virtual Node* Create(const std::string &name, const ParameterSet &parameters) {
      return ((Node*) new T(name,parameters));
   }
};


#endif
