// Copyright (C) 1999 Jean-Marc Valin

#include "GMMScore.h"
#include "net_types.h"
#include "Buffer.h"
#include "covariance.h"
#include "gmm.h"
#include "Vector.h"

DECLARE_NODE(GMMScore)
/*Node
 *
 * @name GMMScore
 * @category HMM
 * @description No description available
 *
 * @input_name FRAMES
 * @input_description No description available
 *
 * @input_name GMM
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


GMMScore::GMMScore(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   gmmInputID = addInput("GMM");
   framesInputID = addInput("FRAMES");
}

void GMMScore::specificInitialize()
{
   processCount = -1;
   this->Node::specificInitialize();
}

void GMMScore::reset()
{
   processCount = -1;
   this->Node::reset();
}

ObjectRef GMMScore::getOutput(int output_id, int count)
{
   //cerr << "Getting output in GMMScore\n";
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         int i;
         NodeInput framesInput = inputs[framesInputID];
         NodeInput gmmInput = inputs[gmmInputID];
         
         ObjectRef inputValue = framesInput.node->getOutput(framesInput.outputID,count);
         //FUTURE: return the right "empty object" instead of inputValue (for buffer reasons)
         Vector<float> &inputFrame = object_cast<Vector<float> > (inputValue);
         
         GMM &gmm = object_cast<GMM> (gmmInput.node->getOutput(gmmInput.outputID,count));
         float score = -gmm.score(&inputFrame[0]).score;
         //for (i=0; i<mat.ncols(); i++)
         //   cerr << mat[count][i] << " ";
         //cerr << endl;
         //cout << "score: " << score << endl;
         
	 //currentScore = ObjectRef(new Float(score));
	 currentScore = Float::alloc(score);
         processCount=count;
      }
      //cerr << "GMMScore returning: " << currentScore << " (" << typeid(currentScore).name() << ")" << endl;
      return currentScore;
   }
   else 
      throw new NodeException (this, "GMMScore: Unknown output id", __FILE__, __LINE__);
}
