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

#include "GMMTrain.h"
#include "net_types.h"
#include "covariance.h"
#include "gmm.h"
#include "Vector.h"

DECLARE_NODE(GMMTrain)
/*Node
 *
 * @name GMMTrain
 * @category VQ
 * @description No description available
 *
 * @input_name FRAMES
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


GMMTrain::GMMTrain(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   //cerr << "GMMTrain initialize\n";
   outputID = addOutput("OUTPUT");
   framesInputID = addInput("FRAMES");
}

void GMMTrain::specificInitialize()
{
   processCount = -1;
   this->Node::specificInitialize();
}

void GMMTrain::reset()
{
   processCount = -1;
   this->Node::reset();
}

ObjectRef GMMTrain::getOutput(int output_id, int count)
{
   //cerr << "Getting output in GMMTrain\n";
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         int i;
         NodeInput framesInput = inputs[framesInputID];

         ObjectRef matRef = framesInput.node->getOutput(framesInput.outputID,count);

         Vector<ObjectRef>  &mat = object_cast<Vector<ObjectRef> > (matRef);

         GMM *gmm = new GMM(1, object_cast <Vector<float> > (mat[0]).size(), NewDiagonalCovariance);
         //cerr << "mat.size(): " << mat.getCurrentPos() << endl;
         vector <float *> data(mat.size());
         for (i=0;i<mat.size();i++)
            data[i]=&object_cast <Vector<float> > (mat[i])[0];
         gmm->init(data);
         gmm->to_real();
         for (i=0;i<5;i++)
         {
            //gmm->split1();
            gmm->binary_split();
            gmm->kmeans1(data,10);
            //cerr << "*******  " << i << "  *******"<<endl;
         }
         //cerr << endl;
         gmm->kmeans1(data,10);

         currentGMM = ObjectRef(gmm);
      }
      return currentGMM;
   }
   else 
      throw new NodeException (this, "GMMTrain: Unknown output id", __FILE__, __LINE__);
}
