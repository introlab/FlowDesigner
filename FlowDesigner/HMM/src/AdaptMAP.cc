// Copyright (C) 2001 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "gmm.h"

using namespace std;
using namespace FD;

class AdaptMAP;

DECLARE_NODE(AdaptMAP)
/*Node
 *
 * @name AdaptMAP
 * @category HMM
 * @require GMM
 * @description Performs MAP adaptation (well, almost!)
 *
 * @input_name FRAMES
 * @input_type Vector<float>
 * @input_description Frame buffer
 *
 * @input_name GMM
 * @input_type GMM
 * @input_description GMM to be adapted
 *
 * @output_name OUTPUT
 * @output_type GMM
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
