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
#include "GrowingBuffer.h"
#include "RotatingBuffer.h"

BufferedNode::BufferedNode(string nodeName, const ParameterSet &params)
   : Node(nodeName, params)
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
   for (int i=0;i<outputs.size();i++)
   {
      if (outputs[i].cacheAll)
      {
         outputs[i].buffer = ObjectRef(new GrowingBuffer (outputs[i].lookAhead+outputs[i].lookBack+1));
      } else {
         outputs[i].buffer = ObjectRef(new RotatingBuffer (outputs[i].lookAhead+outputs[i].lookBack+1));
      }
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
      req.add("LOOKAHEAD", ObjectRef(new Int(inputsCache[i].lookAhead+outputLookAhead)));
      req.add("LOOKBACK", ObjectRef(new Int(inputsCache[i].lookBack+outputLookBack)));
      inputs[i].node->request(inputs[i].outputID, req);
   }
}

void BufferedNode::specificInitialize()
{
   //cerr << "FrameOperation initialize...\n";
   this->Node::specificInitialize();
   this->initializeBuffers();

   this->performRequests();
}

void BufferedNode::reset()
{
   this->Node::reset();
   this->initializeBuffers();
}

void BufferedNode::request(int outputID, const ParameterSet &req)
{
   if (req.exist("LOOKAHEAD"))
      outputs[outputID].lookAhead = max(outputs[outputID].lookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
   if (req.exist("LOOKBACK"))
      outputs[outputID].lookBack = max(outputs[outputID].lookBack,dereference_cast<int> (req.get("LOOKBACK")));
   if (req.exist("CACHEALL"))
      outputs[outputID].cacheAll = true;
   this->Node::request(outputID,req);
}

ObjectRef BufferedNode::getOutput(int output_id, int count)
{
   try {
      Buffer &outBuffer = object_cast<Buffer> (outputs[output_id].buffer);
      
      ObjectRef result = outBuffer[count];
      if (result->status == Object::valid)
         return result;
      else
      {
         calculate (output_id, count, outBuffer);
         if (count > processCount)
            processCount = count;
         return outBuffer[count];
      }
   } catch (BaseException &e)
   {
      e.print();
      throw NodeException (this, "Exception caught in BufferedNode::getOutput", __FILE__, __LINE__);
   }
}
