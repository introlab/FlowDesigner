// Copyright (C) 2001 Jean-Marc Valin
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
#include "Buffer.h"
#include "Vector.h"
#include "gmm.h"

class AdaptMAP;

DECLARE_NODE(AdaptMAP)
/*Node
 *
 * @name AdaptMAP
 * @category HMM
 * @description Performs MAP adaptation (well, almost!)
 *
 * @input_name FRAMES
 * @input_type Vector
 * @input_description Frame buffer
 *
 * @input_name GMM
 * @input_type GMM
 * @input_description GMM to be adapted
 *
 * @output_name OUTPUT
 * @output_type float
 * @output_description Adapted GMM
 *
END*/


class AdaptMAP : public BufferedNode {
   
   int framesID;
   int gmmID;
   int outputID;

public:
   AdaptMAP(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      framesID = addInput("FRAMES");
      gmmID = addInput("GMM");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef gmmValue = getInput(gmmID, count);

      ObjectRef framesValue = getInput(framesID, count);


      Vector<ObjectRef>  &mat = object_cast<Vector<ObjectRef> > (framesValue);
      
      GMM &gmm = object_cast<GMM> (gmmValue);
      vector <float *> data(mat.size());
      for (int i=0;i<mat.size();i++)
	 data[i]= &object_cast <Vector<float> > (mat[i])[0];

      gmm.adaptMAP(data,&gmm);
	 
      out[count] = gmmValue;

      
   }

      
};
