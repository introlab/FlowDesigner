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
#include "gmm.h"
#include "DiagGMM.h"

class MakeDiagGMM;

DECLARE_NODE(MakeDiagGMM)
/*Node
 *
 * @name MakeDiagGMM
 * @category HMM
 * @description Transforms a GMM into a DiagGMM
 *
 * @input_name INPUT
 * @input_type GMM
 * @input_description Input GMM
 *
 * @output_name OUTPUT
 * @output_type DiagGMM
 * @output_description Output DiagGMM
 *
END*/


class MakeDiagGMM : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   MakeDiagGMM(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
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
      
      GMM &gmm = object_cast<GMM> (inputValue);
      out[count] = ObjectRef(gmm.createDiagGMM());
      
   }

      
};
