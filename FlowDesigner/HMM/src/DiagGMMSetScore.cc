// Copyright (C) 2001 Jean-Marc Valin

#include "BufferedNode.h"
#include "Vector.h"
#include "DiagGMM.h"

class DiagGMMSetScore;

DECLARE_NODE(DiagGMMSetScore)
/*Node
 *
 * @name DiagGMMSetScore
 * @category HMM
 * @require DGMM
 * @description Scores a DiagGMM
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @input_name GMM
 * @input_type DiagGMM
 * @input_description Input GMM set (pdf's)
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description GMM scores
 *
END*/


class DiagGMMSetScore : public BufferedNode {
   
   int inputID;
   int gmmID;
   int outputID;

public:
   DiagGMMSetScore(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      gmmID = addInput("GMM");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);


      ObjectRef gmmValue = getInput(gmmID, count);


      Vector<ObjectRef> &gmms = object_cast<Vector<ObjectRef> > (gmmValue);

      //DiagGMM &gmm = object_cast<DiagGMM> (gmmValue);
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(gmms.size());
      out[count] = &output;

      for (int i=0;i<gmms.size();i++)
      {
	 //cerr << "a\n";
	 DiagGMM &gmm = object_cast<DiagGMM> (gmms[i]);
	 if (inputLength != gmm.getDim())
	    throw new NodeException(this, "Dimension mismatch", __FILE__, __LINE__);
	 
	 output[i] = gmm.score(&in[0]);
	 //cerr << "b\n";
      }
      //cerr << "end\n";
   }

      
};
