// Copyright (C) 1999 Jean-Marc Valin

#include "net_types.h"
#include "RBF.h"
#include "Vector.h"
#include "BufferedNode.h"

using namespace std;

namespace FD {

class RBFTrain;
DECLARE_NODE(RBFTrain)
/*Node
 *
 * @name RBFTrain
 * @category VQ
 * @description No description available
 *
 * @input_name FRAMES
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name NB_GAUSSIANS
 * @parameter_description No description available
 *
END*/


class RBFTrain : public BufferedNode {
      
  protected:
      
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'frames' input*/
   int framesInputID;

   /**Number of means to train model*/
   int nb_gaussians;

  public:

   RBFTrain(string nodeName, ParameterSet params) 
   : BufferedNode(nodeName, params)
   { 
      try {
	 //cerr << "RBFTrain initialize\n";
	 outputID = addOutput("OUTPUT");
	 framesInputID = addInput("FRAMES");
	 //cerr << "RBFTrain initialization done\n";

	 nb_gaussians = dereference_cast<int> (parameters.get("NB_GAUSSIANS"));

      } catch (BaseException *e)
      {
	 //e->print(cerr);
	 throw e->add(new NodeException(NULL, "Exception caught in RBFTrain constructor", __FILE__, __LINE__));
      }
   }


   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
   {
      bool binary = false;
      if (parameters.exist("BINARY"))
         binary = dereference_cast<bool> (parameters.get("BINARY"));
      
      NodeInput framesInput = inputs[framesInputID];
	    
      cerr << "getting frames..." << endl;
      ObjectRef matRef = framesInput.node->getOutput(framesInput.outputID,count);
      cerr << "got frames..." << endl;
      Vector<ObjectRef>  &mat = object_cast<Vector<ObjectRef> > (matRef);
	    
      RBF *rbf = new RBF();
	    
      vector <float *> data(mat.size());
      for (size_t i=0;i<mat.size();i++)
         data[i]= &object_cast <Vector<float> > (mat[i])[0];
      int length = (int) object_cast <Vector<float> > (mat[0]).size();
	    
      cerr << "training..." << endl;
      rbf->train(nb_gaussians, data,length,binary);
      cerr << "training complete." << endl;

      out[count] = ObjectRef(rbf);
   }
      

};
}//namespace FD
