// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "net_types.h"
#include "covariance.h"
#include "gmm.h"
#include "Vector.h"

using namespace std;
using namespace FD;

class GMMTrain;
DECLARE_NODE(GMMTrain)
/*Node
 *
 * @name GMMTrain
 * @category HMM
 * @description Trains a GMM using an accumulator of frames
 *
 * @input_name FRAMES
 * @input_type Vector<ObjectRef>
 * @input_description Frame Accumulator
 *
 * @output_name OUTPUT
 * @output_type GMM
 * @output_description The trained GMM
 *
 * @parameter_name SPLIT_LEVELS
 * @parameter_type int
 * @parameter_description Number of times to perform the split = log2 (number of gaussians)
 * 
END*/


class GMMTrain : public BufferedNode {

protected:
   
   int framesInputID;
   int outputID;

   int splitLevels;
      
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   GMMTrain(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      framesInputID = addInput("FRAMES");
   splitLevels = dereference_cast<int> (parameters.get("SPLIT_LEVELS"));
   }
      

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
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
      for (i=0;i<splitLevels;i++)
      {
         //gmm->split1();
         gmm->binary_split();
         gmm->kmeans1(data,20);
         cerr << "*******  " << i << "  *******"<<endl;
      }
      //cerr << endl;
      gmm->kmeans1(data,20);

      out[count] = ObjectRef(gmm);

   }

};

