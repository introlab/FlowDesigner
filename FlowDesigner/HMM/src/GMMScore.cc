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

#include "GMMScore.h"
#include "net_types.h"
#include "Buffer.h"
#include "covariance.h"
#include "gmm.h"
#include "Vector.h"
#include "multithread.h"

//DECLARE_NODE(GMMScore)
NODE_INFO(GMMScore,"VQ", "FRAMES:GMM", "OUTPUT", "")

GMMScore::GMMScore(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   gmmInputID = addInput("GMM");
   framesInputID = addInput("FRAMES");
}

void GMMScore::specificInitialize()
{
   this->Node::specificInitialize();
}

void GMMScore::reset()
{
   this->Node::reset();
}

ObjectRef GMMScore::getOutput(int output_id, int count)
{
   //cerr << "Getting output in GMMScore\n";
   if (output_id==outputID)
   {
      lock();
      if (count != processCount)
      {
         int i;
         NodeInput framesInput = inputs[framesInputID];
         NodeInput gmmInput = inputs[gmmInputID];
         
         ObjectRef inputValue = framesInput.node->getOutput(framesInput.outputID,count);
         //FUTURE: return the right "empty object" instead of inputValue (for buffer reasons)
         if (inputValue->status)
            return inputValue;
         Vector<float> &inputFrame = object_cast<Vector<float> > (inputValue);
         
         GMM &gmm = object_cast<GMM> (gmmInput.node->getOutput(gmmInput.outputID,count));

         float score = gmm.score(inputFrame.begin()).score;
         //for (i=0; i<mat.ncols(); i++)
         //   cerr << mat[count][i] << " ";
         //cerr << endl;
         //cout << "score: " << score << endl;
         currentScore = ObjectRef(new Float(score));
         processCount=count;
      }
      //cerr << "GMMScore returning: " << currentScore << " (" << typeid(currentScore).name() << ")" << endl;
      return unlock_and_return(currentScore);
   }
   else 
      throw NodeException (this, "GMMScore: Unknown output id", __FILE__, __LINE__);
}
