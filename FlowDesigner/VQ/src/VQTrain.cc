// Copyright (C) 1999 Jean-Marc Valin

#include "VQTrain.h"
#include "net_types.h"
#include "kmeans.h"
#include "Vector.h"

DECLARE_NODE(VQTrain)
/*Node
 *
 * @name VQTrain
 * @category VQ
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


VQTrain::VQTrain(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   try {
      //cerr << "VQTrain initialize\n";
      outputID = addOutput("OUTPUT");
      framesInputID = addInput("FRAMES");
      //cerr << "VQTrain initialization done\n";
      nbMeans = dereference_cast<int> (parameters.get("MEANS"));
   } catch (BaseException *e)
   {
      //e->print(cerr);
      throw e->add(new NodeException(NULL, "Exception caught in VQTrain constructor", __FILE__, __LINE__));
   }
}

void VQTrain::specificInitialize()
{
   processCount=-1;
   this->Node::specificInitialize();
}

void VQTrain::reset()
{
   processCount=-1;
   this->Node::reset();
}

ObjectRef VQTrain::getOutput(int output_id, int count)
{
   //cerr << "Getting output in VQTrain\n";
   if (output_id==outputID)
   {
      if (count != processCount)
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

         current = ObjectRef(vq);
      }
      return current;
   }
   else 
      throw new NodeException (this, "VQTrain: Unknown output id", __FILE__, __LINE__);
}
