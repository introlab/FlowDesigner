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
#include "RBF.h"
#include "Vector.h"
#include "Node.h"
#include <strstream>

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


class RBFTrain : public Node {
      
  protected:
      
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'frames' input*/
   int framesInputID;

   /**Reference to the current object*/
   ObjectRef current;

   /**Number of means to train model*/
   int nb_gaussians;

   int processCount;

  public:

   RBFTrain(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
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

   void specificInitialize()
   {
      processCount=-1;
      this->Node::specificInitialize();
   }

   void reset()
   {
      processCount=-1;
      this->Node::reset();
   }

   ObjectRef getOutput(int output_id, int count)
   {
      //cerr << "Getting output in RBFTrain\n";
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
	    
	    RBF *rbf = new RBF();
	    
	    vector <float *> data(mat.size());
	    for (i=0;i<mat.size();i++)
	       data[i]= &object_cast <Vector<float> > (mat[i])[0];
	    int length = object_cast <Vector<float> > (mat[0]).size();
	    
	    cerr << "training..." << endl;
	    rbf->train(nb_gaussians, data,length,binary);
	    cerr << "training complete." << endl;

	    current = ObjectRef(rbf);
	 }
	 return current;
      }
      else 
	 throw new NodeException (this, "RBFTrain: Unknown output id", __FILE__, __LINE__);
   }
      
  protected:
   /**Default constructor, should not be used*/
   RBFTrain() {throw new GeneralException("RBFTrain copy constructor should not be called",__FILE__,__LINE__);}

};
