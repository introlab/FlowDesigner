// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "Matrix.h"
#include "Vector.h"

using namespace std;

namespace FD {

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
 * @parameter_name THRESHOLD
 * @parameter_description The minimum transition probability allowed
 *
END*/


class TransMatrix : public BufferedNode {

protected:
   
   int inputID;

   int outputID;

   int nbStates;

   float threshold;
public:
   
   TransMatrix(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      inputID = addInput("INPUT");
      
      nbStates = dereference_cast<int> (parameters.get("NB_STATES"));

      threshold = parameters.exist("THRESHOLD") ? dereference_cast<float> (parameters.get("THRESHOLD")) : 0;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef statesValue = getInput(inputID, count);
      Vector<ObjectRef>  &states = object_cast<Vector<ObjectRef> > (statesValue);

      Matrix<float> &trans = *new Matrix<float> (nbStates, nbStates);
      out[count] = ObjectRef(&trans);
      
      DYN_VEC(int, nbStates, sums);
      //int sums[nbStates];
      for (int i=0;i<nbStates;i++)
	 sums[i] = 0;
      
      for (int i=0;i<states.size()-1;i++)
      {
	 Vector<float> &from = object_cast<Vector<float> > (states[i]);
	 Vector<float> &to = object_cast<Vector<float> > (states[i+1]);
	 trans(to[0],from[0]) += 1;
	 sums[int(from[0])]++;
      }
      
      for (int i=0;i<nbStates;i++)
	 for (int j=0;j<nbStates;j++)
	 {
	    trans(i,j) /= sums[j];
	    if (trans(i,j) < threshold) 
	       trans(i,j) = threshold;
	 }
   }
      

protected:
   /**Default constructor, should not be used*/
   TransMatrix() {throw new GeneralException("TransMatrix copy constructor should not be called",__FILE__,__LINE__);}

};

}//namespace FD
