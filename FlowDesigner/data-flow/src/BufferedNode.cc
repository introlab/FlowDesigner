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

#include "BufferedNode.h"
//#include "UINode.h"

//#include "RotatingBuffer.h"

BufferedNode::BufferedNode(string nodeName, const ParameterSet &params)
   : Node(nodeName, params)
   , inOrder(false)
{}

int BufferedNode::addInput (const string &inputName)
{
   int inID = this->Node::addInput(inputName);
   if (inID >= inputsCache.size())
      inputsCache.resize(inID+1);
   return inID;
}

int BufferedNode::addOutput(const string &output_name)
{
   /*handle output buffers here*/
   int outID = this->Node::addOutput(output_name);
   if (outID >= outputs.size())
      outputs.resize(outID+1);
   return outID;
}

void BufferedNode::initializeBuffers()
{
   //cerr << typeid(*this).name() << " " << outputs[0].lookAhead << " " << outputs[0].lookBack << endl;

   for (int i=0;i<outputs.size();i++)
   {
      outputs[i].buffer = RCPtr<Buffer>(new Buffer (outputs[i].lookAhead+outputs[i].lookBack+1));
   }
}

void BufferedNode::performRequests ()
{
   int i;
   int outputLookAhead=0, outputLookBack=0;
   for (i=0;i<outputs.size();i++)
   {
      outputLookAhead=max(outputLookAhead, outputs[i].lookAhead);
      outputLookBack =max(outputLookBack, outputs[i].lookBack);
   }
   
   for (i=0;i<inputsCache.size();i++)
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

   Node *ptr=this;
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
	 return outBuffer[count];
      } else {
	 if (count > outBuffer.getCurrentPos())
	 {
	    calculate (output_id, count, outBuffer);
	    return outBuffer[count];
	 } else {
	    ObjectRef value = outBuffer[count];
	    if (value->status != Object::valid)
	       calculate (output_id, count, outBuffer);
	    return value;
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
