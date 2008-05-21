// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "net_types.h"
#include "kmeans.h"
#include "Vector.h"

using namespace std;

namespace FD {

class VQTrain;
DECLARE_NODE(VQTrain)
/*Node
 *
 * @name VQTrain
 * @category VQ
 * @require VQ
 * @description No description available
 *
 * @input_name FRAMES
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name MEANS
 * @parameter_description No description available
 *
 * @parameter_name BINARY
 * @parameter_description No description available
 *
END*/


class VQTrain : public BufferedNode {

protected:
   
   int framesInputID;
   int outputID;

   int nbMeans;
      
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   VQTrain(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      framesInputID = addInput("FRAMES");
      nbMeans = dereference_cast<int> (parameters.get("MEANS"));
   }
      

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
   {
      bool binary = false;
      if (parameters.exist("BINARY"))
         binary = dereference_cast<bool> (parameters.get("BINARY"));
      int i;
      NodeInput framesInput = inputs[framesInputID];
      
      cerr << "getting frames..." << endl;
      ObjectRef matRef = framesInput.node->getOutput(framesInput.outputID,count);
      cerr << "got frames..." << endl;
      Vector<ObjectRef>  &mat = object_cast<Vector<ObjectRef> > (matRef);
      
      KMeans *vq = new KMeans;
      
      vector <float *> data(mat.size());
      for (i=0;i<mat.size();i++)
         data[i]= &object_cast <Vector<float> > (mat[i])[0];
      int length = object_cast <Vector<float> > (mat[0]).size();
      
      cerr << "training..." << endl;
      vq->train(nbMeans,data,length,binary);
      cerr << "training complete." << endl;
      out[count] = ObjectRef(vq);
   }

};
}//namespace FD
