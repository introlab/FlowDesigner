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

#include "MMIScore.h"
#include "net_types.h"
#include "Buffer.h"
#include "Cell.h"
#include "Vector.h"
#include "multithread.h"

//DECLARE_NODE(MMIScore)
NODE_INFO(MMIScore,"VQ", "FRAMES:MMI", "OUTPUT", "")

MMIScore::MMIScore(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   mmiInputID = addInput("MMI");
   framesInputID = addInput("FRAMES");
}

void MMIScore::specificInitialize()
{
   this->Node::specificInitialize();
}

void MMIScore::reset()
{
   this->Node::reset();
}

void MMIScore::request(int outputID, const ParameterSet &req) {inputs[framesInputID].node->request(outputID,req);}

ObjectRef MMIScore::getOutput(int output_id, int count)
{
   //cerr << "Getting output in MMIScore with count = " << count << endl;
   if (output_id==outputID)
   {
      lock();
      if (count != processCount)
      {
         int i;
         NodeInput framesInput = inputs[framesInputID];
         NodeInput mmiInput = inputs[mmiInputID];
         
         ObjectRef inputValue = framesInput.node->getOutput(framesInput.outputID,count);
         //FUTURE: return the right "empty object" instead of inputValue (for buffer reasons)
         if (inputValue->status)
            return inputValue;
         Vector<float> &inputFrame = object_cast<Vector<float> > (inputValue);
         
         Cell &mmi = object_cast<Cell> (mmiInput.node->getOutput(mmiInput.outputID,count));

         int cellID = mmi.belongs(inputFrame.begin());
         //cerr << "Cell: " << cellID << endl;
         currentScore = ObjectRef(new Int(cellID));
         processCount=count;
      }
      //cerr << "MMIScore returning: " << currentScore << " (" << typeid(currentScore).name() << ")" << endl;
      return unlock_and_return(currentScore);
   }
   else 
      throw new NodeException (this, "MMIScore: Unknown output id", __FILE__, __LINE__);
}
