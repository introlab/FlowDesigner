// Copyright (C) 2001 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "BufferedNode.h"
#include "Vector.h"
#include "DiagGMM.h"

class DiagGMMScore;

DECLARE_NODE(DiagGMMScore)
/*Node
 *
 * @name DiagGMMScore
 * @category HMM
 * @description Scores a DiagGMM
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input vector
 *
 * @input_name GMM
 * @input_type DiagGMM
 * @input_description Input GMM (pdf)
 *
 * @output_name OUTPUT
 * @output_type Float
 * @output_description GMM score
 *
END*/


class DiagGMMScore : public BufferedNode {
   
   int inputID;
   int gmmID;
   int outputID;

public:
   DiagGMMScore(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      gmmID = addInput("GMM");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }

      ObjectRef gmmValue = getInput(gmmID, count);

      if (gmmValue->status != Object::valid)
      {
	 out[count] = gmmValue;
         return;
      }
       
      DiagGMM &gmm = object_cast<DiagGMM> (gmmValue);
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();
      if (inputLength != gmm.getDim())
	 throw new NodeException(this, "Dimension mismatch", __FILE__, __LINE__);

      out[count] = ObjectRef(Float::alloc(gmm.score(&in[0])));
      
   }

      
};
