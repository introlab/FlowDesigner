// Copyright (C) 1999 Jean-Marc Valin
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
#include "GCMS.h"
#include "FrameOperation.h"
#include "Vector.h"
#include "multithread.h"
#include "Buffer.h"
#include "RotatingBuffer.h"

//DECLARE_NODE(GCMS)
NODE_INFO(GCMS,"Signal", "INPUT", "OUTPUT", "LENGTH")

GCMS::GCMS(string nodeName, const ParameterSet &params) 
   : FrameOperation(nodeName, params) 
   , sum(outputLength, float ())
   , accumCount(0)
{
   inputID = addInput("INPUT");
   if (parameters.exist("INPUTLENGTH"))
      inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
   else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
}

void GCMS::specificInitialize()
{
   //cerr << "GCMS initialize...\n";
   this->FrameOperation::specificInitialize();
   
   for (int i=0;i<outputLength;i++)
      sum[i]=0;
   accumCount=0;
}

void GCMS::reset()
{
   this->FrameOperation::reset();

   for (int i=0;i<outputLength;i++)
      sum[i]=0;
   accumCount=0;
}

ObjectRef GCMS::getOutput(int output_id, int count)
{
   lock();
   try {
      Buffer &out = object_cast<Buffer> (output);
      
      if (count != processCount)
      {
         int i;
         ObjectRef inputValue;
         processCount = count;
         NodeInput input = inputs[inputID];
         
         inputValue = input.node->getOutput(input.outputID, count);
         if (inputValue->status != Object::valid)
         {
            return inputValue;
         }
         //cerr << "input invalid? " << invalid<< endl;
         
         
         //cerr << "computing\n";
         //this->computeFrame(inputBuffer, count);
         Vector<float> &cms = object_cast<Vector<float> > (out[count]);
         Vector<float> &in = object_cast<Vector<float> > (inputValue);
         accumCount++;
         float inv_accum=1.0/accumCount;
         for (i=0;i<outputLength;i++)
         {
            sum[i] = (1-inv_accum)*sum[i] + inv_accum*in[i];
            cms[i] = in[i]-sum[i];
           
         }
         out[count]->status = Object::valid;
      }
      
      //unlock();
      //cerr << "leaving GCMS::getOutput for " << name << " count: " << count << endl;
      //cerr << "returning status " << out[count]->status << endl;
      return unlock_and_return(out[count]);
   } catch (BaseException &e)
   {
      e.print();
      unlock();
      throw NodeException (this, "Exception in GCMS::getOutput", __FILE__, __LINE__);
   }
}
