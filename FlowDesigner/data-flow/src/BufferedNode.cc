// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "BufferedNode.h"
//#include "UINode.h"

//#include "RotatingBuffer.h"

BufferedNode::BufferedNode(string nodeName, const ParameterSet &params)
   : Node(nodeName, params)
   , inOrder(false)
{}

int BufferedNode::addInput (const string &inputName)
{
   unsigned int inID = this->Node::addInput(inputName);
   if (inID >= inputsCache.size())
      inputsCache.resize(inID+1);
   return inID;
}

int BufferedNode::addOutput(const string &output_name)
{
   /*handle output buffers here*/
   unsigned int outID = this->Node::addOutput(output_name);
   if (outID >= outputs.size())
      outputs.resize(outID+1);
   return outID;
}

void BufferedNode::initializeBuffers()
{
   //cerr << typeid(*this).name() << " " << outputs[0].lookAhead << " " << outputs[0].lookBack << endl;

   for (unsigned int i=0;i<outputs.size();i++)
   {
      outputs[i].buffer = RCPtr<Buffer>(new Buffer (outputs[i].lookAhead+outputs[i].lookBack+1));
   }
}

void BufferedNode::performRequests ()
{
   int outputLookAhead=0, outputLookBack=0;
   for (unsigned i=0;i<outputs.size();i++)
   {
      outputLookAhead=max(outputLookAhead, outputs[i].lookAhead);
      outputLookBack =max(outputLookBack, outputs[i].lookBack);
   }
   
   for (unsigned i=0;i<inputsCache.size();i++)
   {
      ParameterSet req;
      //cerr << inputsCache[i].lookAhead+outputLookAhead << endl;
      req.add("LOOKAHEAD", ObjectRef(new Int(inputsCache[i].lookAhead+outputLookAhead)));
      req.add("LOOKBACK", ObjectRef(new Int(inputsCache[i].lookBack+outputLookBack)));
      inputs[i].node->request(inputs[i].outputID, req);
   }
}

void BufferedNode::specificInitialize()
{
   processCount = -1;
   this->Node::specificInitialize();
   this->initializeBuffers();
   this->performRequests();
}

void BufferedNode::reset()
{
   processCount = -1;
   this->Node::reset();
   this->initializeBuffers();
}

void BufferedNode::request(int outputID, const ParameterSet &req)
{
   //cerr << "name = " << name << " this = " << this << " outputID = " << outputID << endl;   cerr << "lookahead = " << outputs[outputID].lookAhead << " lookback = " << outputs[outputID].lookBack << endl;   
   
   if (req.exist("LOOKAHEAD"))
      outputs[outputID].lookAhead = max(outputs[outputID].lookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
   if (req.exist("LOOKBACK"))
      outputs[outputID].lookBack = max(outputs[outputID].lookBack,dereference_cast<int> (req.get("LOOKBACK")));
   if (req.exist("INORDER"))
      inOrder = true;
   this->Node::request(outputID,req);

   //Node *ptr=this;
   //cerr << "request caught in " << name << " " << typeid(*ptr).name() << endl;
   //cerr << "lookahead = " << outputs[outputID].lookAhead << " lookback = " << outputs[outputID].lookBack << endl;
   //cerr << "--\n";
}

ObjectRef BufferedNode::getOutput(int output_id, int count)
{
   try 
   {
      Buffer &outBuffer = *(outputs[output_id].buffer);
      if (inOrder)
      {
	 for (int i=processCount+1;i<=count;i++)
	    calculate (output_id, i, outBuffer);
         if (count > processCount)
            processCount = count;
	 return outBuffer.get(count);
      } else {
	 if (count > outBuffer.getCurrentPos())
	 {
	    calculate (output_id, count, outBuffer);
	    //return outBuffer[count];
	    return outBuffer.get(count);
	 } else {
	    if (!outBuffer.isValid(count))
	    {
	       calculate (output_id, count, outBuffer);
	    }
	    return outBuffer.get(count);
	    
	 }
      }
      
   } catch (BaseException *e)
   {
      //e->print();
      //cerr << "error caught in" << typeid(*this).name() << endl;
      //uinode->notifyError(string("tata"));
      throw e->add(new NodeException (this, "Exception caught in BufferedNode::getOutput", __FILE__, __LINE__));
   }
}
