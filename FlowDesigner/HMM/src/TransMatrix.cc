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


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "Matrix.h"
#include "Buffer.h"
#include "Vector.h"

class TransMatrix;

DECLARE_NODE(TransMatrix)
/*Node
 *
 * @name TransMatrix
 * @category HMM
 * @description No description available
 *
 * @input_name INPUT
 * @input_description state numbers in a frame buffer
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name NB_STATES
 * @parameter_description Number of HMM states
 *
END*/


class TransMatrix : public BufferedNode {

protected:
   
   int inputID;

   int outputID;

   int nbStates;
public:
   
   TransMatrix(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      inputID = addInput("INPUT");
      
      nbStates = dereference_cast<int> (parameters.get("NB_STATES"));
            
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef statesValue = getInput(inputID, count);
      Buffer &states = object_cast<Buffer> (statesValue);

      Matrix<float> &trans = *new Matrix<float> (nbStates, nbStates);
      out[count] = ObjectRef(&trans);
      
      int sums[nbStates];
      for (int i=0;i<nbStates;i++)
	 sums[i] = 0;
      
      for (int i=0;i<states.getCurrentPos()-1;i++)
      {
	 Vector<float> &from = object_cast<Vector<float> > (states[i]);
	 Vector<float> &to = object_cast<Vector<float> > (states[i+1]);
	 trans(to[0],from[0]) += 1;
	 sums[int(from[0])]++;
      }
      
      for (int i=0;i<nbStates;i++)
	 for (int j=0;j<nbStates;j++)
	    trans(i,j) /= sums[j];
   }
      

protected:
   /**Default constructor, should not be used*/
   TransMatrix() {throw new GeneralException("TransMatrix copy constructor should not be called",__FILE__,__LINE__);}

};
