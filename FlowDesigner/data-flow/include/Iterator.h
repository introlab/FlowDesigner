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
#ifndef _ITERATOR_H_
#define _ITERATOR_H_

#include "Network.h"
#include "CollectorNode.h"

/** Input translator node used only for the Iterator class */
class InputTranslator : public CollectorNode {
   
public:
   
   /** The constructor with a nodeName and parameters */
   InputTranslator (string nodeName, ParameterSet params) 
      :CollectorNode(nodeName, params) {
      //nothing to do
      ;
   }
   
   /** The special case where we known which processCount to use */
   void setProcessCount(int pc) {processCount = pc;}
   
   /** Returns the current processCount of this node */
   int  getProcessCount() {return processCount;}
   
   /** The getOutput method overloaded from CollectorNode */
   virtual ObjectRef getOutput (int output_id, int count) {
      //changed count for processCount
      return CollectorNode::getOutput(output_id,processCount);
   }
   
private:
   
   /** The default constructor that should never be used */
   InputTranslator () {
      throw NodeException (NULL,"The default constructor should not be called from InputTranslator",__FILE__,__LINE__);
   }
};

/** Iterator Node */
class Iterator : public Network {
   
public:
   
   /** The constructor with a nodeName and parameters */
   Iterator (string nodeName, ParameterSet params);
   
   /** The getOutput method overloaded from Node */
   virtual ObjectRef getOutput (int output_id, int count);
   
   /** The connectToNode method overloaded from Node */
   virtual void connectToNode(string in, Node *inNode, string out) {
      cout<<"Iterator : Calling connectToNode"<<endl;
      if (!inputNode) {
         throw NodeException(this,string("No input node in iterator :") + name, __FILE__,__LINE__);
      }
      connectToNode(inputNode->translateInput(in), inNode, inNode->translateOutput(out));      
   }

   /** setting the condition Node */
   void setConditionNode(Node* aNode) {conditionNode = aNode;}
   
   /** returning the condition Node */
   Node* getConditionNode() {return conditionNode;}

   /**Subnet : NetworkNode specific initialize*/
   virtual void specificInitialize();

protected:
   
   /** Connect an input node using numeric (integer) input/output names*/
   virtual void connectToNode(unsigned int in, Node *inNode, unsigned int out);
   
private:

   /** It true, the iterator is a do; while()  (the condition is tested last)*/
   bool doWhile;

   /** Our special conditionNode*/
   Node *conditionNode;
   
   /** Our special translator node */
   InputTranslator *translator;
   
   /** The output of the iterator*/
   ObjectRef output;
   
   /** Default constructor that should not be used*/
   Iterator() {
      throw NodeException (NULL,"The default constructor should not be called from Iterator",__FILE__,__LINE__);
   }
   
};
#endif
