// Copyright (C) 1999 Jean-Marc Valin and Dominic Letourneau
#ifndef _ITERATOR_H_
#define _ITERATOR_H_

#include "Network.h"
#include "Collector.h"
#include "BufferedNode.h"

/** Input translator node used only for the Iterator class */
class InputTranslator : public BufferedNode {

protected:

   int processCount;

public:
   
   /** The constructor with a nodeName and parameters */
   InputTranslator (string nodeName, ParameterSet params) 
      :BufferedNode(nodeName, params) {
      //nothing to do
      ;
   }
   
   /** The special case where we known which processCount to use */
   void setProcessCount(int pc) {processCount = pc;}
   
   /** Returns the current processCount of this node */
   int  getProcessCount() {return processCount;}
   
   
   virtual void request(int outputID, const ParameterSet &req) 
   {
      if (req.exist("LOOKAHEAD"))
      outputs[outputID].lookAhead = max(outputs[outputID].lookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
   if (req.exist("LOOKBACK"))
      outputs[outputID].lookBack = max(outputs[outputID].lookBack,dereference_cast<int> (req.get("LOOKBACK")));
   if (req.exist("INORDER"))
      inOrder = true;

     //handled by BufferedNode
   }
   

   virtual void calculate(int output_id, int count, Buffer &out) {

     int outputID = inputs[output_id].outputID;
     
     //same as the collector's job!
     out[count] = (inputs[output_id].node)->getOutput(outputID,processCount);
   }

   int addInput (const string &inputName)
   {
      BufferedNode::addInput(inputName);
      return BufferedNode::addOutput(inputName);
   }

   virtual int translateInput (string inputName) {
     for (unsigned int i=0; i< inputs.size(); i++) {
       if (inputs[i].name == inputName) {
         return i;
       }
     }    
     return addInput(inputName);
   }

   virtual int translateOutput (string outputName) {
     // Simply call translateInput because it should return
     // the same integer...
     return translateInput(outputName);
   }

   virtual bool hasOutput(int output_id) const {
     return(int(inputs.size()) > output_id);
   }
   
   
   virtual void requestForIterator(const ParameterSet &req) 
   {
      //request propagation to nodes before the iterator
      for (int i=0;i<inputs.size();i++) 
      {
         inputs[i].node->request(inputs[i].outputID,req);
      }
      

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
   virtual void initialize();

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
