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

#include "net_types.h"
#include "Buffer.h"
#include "msvq.h"
#include "Vector.h"
#include "Node.h"
#include <strstream>
//#include "multithread.h"

class MSVQTrain;
DECLARE_NODE(MSVQTrain)
/*Node

 * @name MSVQTrain
 * @category VQ
 * @description No description available

 * @input_name FRAMES
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name STAGES
 * @parameter_description No description available

 * @parameter_name BINARY
 * @parameter_description No description available

END*/


class MSVQTrain : public Node {
      
  protected:
      
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'frames' input*/
   int framesInputID;

   /**Reference to the current object*/
   ObjectRef current;

   /**Number of means to train model*/
   vector<int> stages;
  public:

   MSVQTrain(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
   { 
      try {
	 //cerr << "MSVQTrain initialize\n";
	 outputID = addOutput("OUTPUT");
	 framesInputID = addInput("FRAMES");
	 //cerr << "MSVQTrain initialization done\n";

         //stages = ObjectRef(new Vector<int>);
	 //Vector<int> &val = object_cast<Vector<int> > (stages);
	 istrstream str_vector(object_cast <String> (parameters.get("STAGES")).c_str());
	 str_vector >> stages;
	 

	 //nbMeans = dereference_cast<int> (parameters.get("MEANS"));
      } catch (BaseException *e)
      {
	 //e->print(cerr);
	 throw e->add(new NodeException(NULL, "Exception caught in MSVQTrain constructor", __FILE__, __LINE__));
      }
   }

   void specificInitialize()
   {
      this->Node::specificInitialize();
   }

   void reset()
   {
      this->Node::reset();
   }

   ObjectRef getOutput(int output_id, int count)
   {
      //cerr << "Getting output in MSVQTrain\n";
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
	    Buffer &mat = object_cast<Buffer> (matRef);
	    
	    MSVQ *vq = new MSVQ(stages);
	    
	    vector <float *> data(mat.getCurrentPos()+1);
	    for (i=0;i<=mat.getCurrentPos();i++)
	       data[i]= &object_cast <Vector<float> > (mat[i])[0];
	    int length =  object_cast <Vector<float> > (mat[0]).size();
	    
	    cerr << "training..." << endl;
	    vq->train(data,length,binary);
	    cerr << "training complete." << endl;

	    current = ObjectRef(vq);
	 }
	 return current;
      }
      else 
	 throw new NodeException (this, "MSVQTrain: Unknown output id", __FILE__, __LINE__);
   }
      
  protected:
   /**Default constructor, should not be used*/
   MSVQTrain() {throw new GeneralException("MSVQTrain copy constructor should not be called",__FILE__,__LINE__);}

};
