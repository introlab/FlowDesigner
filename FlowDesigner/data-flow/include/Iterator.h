// Copyright (C) 1999 Jean-Marc Valin and Dominic Letourneau
#ifndef _ITERATOR_H_
#define _ITERATOR_H_

#include "Network.h"
#include "Collector.h"

/** Input translator node used only for the Iterator class */
class InputTranslator : public Collector {

protected:

   int processCount;

public:
   
   /** The constructor with a nodeName and parameters */
   InputTranslator (string nodeName, ParameterSet params) 
      :Collector(nodeName, params) {
      //nothing to do
      ;
   }
   
   /** The special case where we known which processCount to use */
   void setProcessCount(int pc) {processCount = pc;}
   
   /** Returns the current processCount of this node */
   int  getProcessCount() {return processCount;}
   
   /** The getOutput method overloaded from Collector */
   virtual ObjectRef getOutput (int output_id, int count) {
      //changed count for processCount
      return Collector::getOutput(output_id,processCount);
   }

   virtual void request(int outputID, const ParameterSet &req) 
   {
   }

   virtual void requestForIterator(const ParameterSet &req) 
   {
      for (int i=0;i<inputs.size();i++)
         inputs[i].node->request(inputs[i].outputID,req);
   }

private:
   
   /** The default constructor that should never be used */
   InputTranslator () {
      throw new NodeException (NULL,"The default constructor should not be called from InputTranslator",__FILE__,__LINE__);
   }
};






/** Iterator Node */
class Iterator : public Network {

protected:

   int processCount;
 
public:
   
   /** The constructor with a nodeName and parameters */
   Iterator (string nodeName, ParameterSet params);
   
   /** The getOutput method overloaded from Node */
   virtual ObjectRef getOutput (int output_id, int count);
   
   /** The connectToNode method overloaded from Node */
   virtual void connectToNode(string in, Node *inNode, string out) {
      if (!inputNode) {
         throw new NodeException(this,string("No input node in iterator :") + name, __FILE__,__LINE__);
      }
      connectToNode(inputNode->translateInput(in), inNode, inNode->translateOutput(out));      
   }

   virtual void request(int outputID, const ParameterSet &req) 
   {
      ParameterSet r;
      Collector *col = dynamic_cast<Collector*> (sinkNode);
      if (col)
         col->requestAll(r);
      else
         throw NodeException(this, "Iterator output should be a collector", __FILE__, __LINE__);
      conditionNode->request(0,r);  
      if (translator)
         translator->requestForIterator(req);
   }

   /** setting the condition Node */
   void setConditionNode(Node* aNode) {conditionNode = aNode;}
   
   /** returning the condition Node */
   Node* getConditionNode() {return conditionNode;}

   /**Iterator specific initialize*/
   virtual void specificInitialize();

   /**Resets the Iterator and all the internal nodes */
   virtual void reset();

   /**Notify the node that is will be destroyed shortly*/
   virtual void stop();

protected:
   
   /** Connect an input node using numeric (integer) input/output names*/
   virtual void connectToNode(unsigned int in, Node *inNode, unsigned int out);
   
   /** It true, the iterator is a do; while()  (the condition is tested last)*/
   bool doWhile;

   bool exit_status;


   /** Our special conditionNode*/
   Node *conditionNode;
   
   /** Our special translator node */
   InputTranslator *translator;
   
   /** The output of the iterator*/
   vector<ObjectRef> output;
   
   /** Default constructor that should not be used*/
   Iterator() {
      throw new NodeException (NULL,"The default constructor should not be called from Iterator",__FILE__,__LINE__);
   }
   
};
#endif
