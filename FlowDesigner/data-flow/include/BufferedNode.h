// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef BUFFERED_NODE_H
#define BUFFERED_NODE_H

#include "Node.h"
#include "Buffer.h"

class OutputCacheInfo {
public:
   OutputCacheInfo () 
      : lookAhead(0)
      , lookBack(0)
   {}

   RCPtr<Buffer> buffer;
   int lookAhead;
   int lookBack;
};

class InputCacheInfo {
public:
   InputCacheInfo () 
      : lookAhead(0)
      , lookBack(0)
   {}
   int lookAhead;
   int lookBack;
};

class BufferedNode : public Node {
protected:

   int processCount;

   vector<OutputCacheInfo> outputs;

   vector<InputCacheInfo> inputsCache;
   
   /**true if the calculations must be made in order of process count*/
   bool inOrder;
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   BufferedNode(string nodeName, const ParameterSet &params);
   
   /**Destructor*/
   virtual ~BufferedNode() {}

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &buf) = 0;

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req);

   //virtual void request(const ParameterSet &req) {throw "error: maudit request";}

   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize();

   /**Resets the node internal values and buffers*/
   virtual void reset();

   /**Adding an output to a node*/
   virtual int addOutput (const string &outputName);

   /**Adding an input to a node*/
   virtual int addInput (const string &inputName);

   /***/
   virtual void initializeBuffers();

   /***/
   virtual void performRequests();

protected:
   /**Default constructor, should not be used*/
   BufferedNode() {throw new GeneralException("BufferedNode copy constructor should not be called",__FILE__,__LINE__);}
};

#ifdef ENABLE_SPEED_HACKS

#define NO_ORDER_NODE_SPEEDUP(nodeClass)                    \
ObjectRef getOutput(int output_id, int count)               \
{                                                           \
      Buffer &outBuffer = *(outputs[output_id].buffer);     \
      if (!outBuffer.isValid(count))                        \
	 nodeClass::calculate (output_id, count, outBuffer);\
      return outBuffer.get(count);}

#define IN_ORDER_NODE_SPEEDUP(nodeClass)                    \
ObjectRef getOutput(int output_id, int count)               \
{                                                           \
      Buffer &outBuffer = *(outputs[output_id].buffer);     \
      for (int i=processCount+1;i<=count;i++)               \
	 nodeClass::calculate(output_id, i, outBuffer);     \
      if (count > processCount)                             \
         processCount = count;                              \
      return outBuffer.get(count);                          \
}


#else


#define NO_ORDER_NODE_SPEEDUP(nodeClass)                    \
ObjectRef getOutput(int output_id, int count)               \
{                                                           \
   try {                                                    \
      Buffer &outBuffer = *(outputs[output_id].buffer);     \
      if (!outBuffer.isValid(count))                        \
	 nodeClass::calculate (output_id, count, outBuffer);\
      return outBuffer.get(count);                          \
   } catch (BaseException *e)                               \
   {                                                        \
      throw e->add(new NodeException (this, "Exception caught in BufferedNode::getOutput", __FILE__, __LINE__));\
   }                                                        \
}

#define IN_ORDER_NODE_SPEEDUP(nodeClass)                    \
ObjectRef getOutput(int output_id, int count)               \
{                                                           \
   try {                                                    \
      Buffer &outBuffer = *(outputs[output_id].buffer);     \
      for (int i=processCount+1;i<=count;i++)               \
	 nodeClass::calculate(output_id, i, outBuffer);     \
      if (count > processCount)                             \
         processCount = count;                              \
      return outBuffer.get(count);                          \
   } catch (BaseException *e)                               \
   {                                                        \
      throw e->add(new NodeException (this, "Exception caught in BufferedNode::getOutput", __FILE__, __LINE__));\
   }                                                        \
}


#endif

#endif
