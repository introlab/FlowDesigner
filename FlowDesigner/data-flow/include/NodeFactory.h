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
   string name;
   string category;
   string inputs;
   string outputs;
   string params;
   
public:
   _NodeFactory(string _name, string _category, string _inputs, string _outputs, string _params)
      : name(_name)
      , category(_category)
      , inputs(_inputs)
      , outputs(_outputs)
      , params(_params)
   {}   
   virtual const string &getName() {return name;}
   virtual const string &getCategory() {return category;}
   vector<string> getInputs() 
   {
      vector<string> ret;
      if (inputs.size() == 0)
         return ret;
      ret.resize(1);
      string *curr=&(ret[0]);
      for (int i=0;i<inputs.size();i++)
      {
         if (inputs[i] != ':')
            curr->insert(curr->end(),inputs[i]);
         else
         {
            ret.resize(ret.size()+1);
            curr=&(ret[ret.size()-1]);
         }
      }
      return ret;
   }
   
   vector<string> getOutputs() 
   {
      vector<string> ret;
      if (outputs.size() == 0)
         return ret;
      ret.resize(1);
      string *curr=&(ret[0]);
      for (int i=0;i<outputs.size();i++)
      {
         if (outputs[i] != ':')
            curr->insert(curr->end(),outputs[i]);
         else
         {
            ret.resize(ret.size()+1);
            curr=&(ret[ret.size()-1]);
         }
      }
      return ret;
   }

   vector<string> getParams() 
   {
      vector<string> ret;
      if (params.size() == 0)
         return ret;
      ret.resize(1);
      string *curr=&(ret[0]);
      for (int i=0;i<params.size();i++)
      {
         if (params[i] != ':')
            curr->insert(curr->end(),params[i]);
         else
         {
            ret.resize(ret.size()+1);
            curr=&(ret[ret.size()-1]);
         }
      }
      return ret;
   }

   virtual Node* Create(const string &name, const ParameterSet &parameters) = 0;
   virtual ~_NodeFactory() {;}

private:


};


//Template class used by all Nodes
template <class T>
class NodeFactory : public _NodeFactory {
public:
   NodeFactory(string _name)
      : _NodeFactory(_name, "Unknown", "INPUT", "OUTPUT", "")
   {}
   virtual Node* Create(const string &name, const ParameterSet &parameters) {
      return ((Node*) new T(name,parameters));
   }
};

//Template class used by all Nodes
template <class T>
class NodeInfo : public _NodeFactory {
public:
   NodeInfo(string _name, string _category, string _inputs, string _outputs, string _params)
      : _NodeFactory(_name, _category, _inputs, _outputs, _params)
   {}
   virtual Node* Create(const string &name, const ParameterSet &parameters) {
      return ((Node*) new T(name,parameters));
   }
};


#endif
