// Copyright (C) 1999 Jean-Marc Valin

#include "MMITrain.h"
#include "net_types.h"
#include "Cell.h"
#include "Vector.h"

DECLARE_NODE(MMITrain)
/*Node
 *
 * @name MMITrain
 * @category VQ
 * @description Train Maximum Mutual Information (MMI) Tree
 *
 * @input_name FRAMES
 * @input_type Vector<ObjectRef> 
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_type Cell
 * @output_description MMI tree
 *
 * @parameter_name LEVELS
 * @parameter_type int
 * @parameter_description Number of levels for the tree
 *
END*/


MMITrain::MMITrain(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   //cerr << "MMITrain initialize\n";
   outputID = addOutput("OUTPUT");
   framesInputID = addInput("FRAMES");
   nb_levels=dereference_cast<int> (parameters.get("LEVELS"));
}

void MMITrain::specificInitialize()
{
   processCount=-1;
   this->Node::specificInitialize();
}

void MMITrain::reset()
{
   processCount=-1;
   this->Node::reset();
}

ObjectRef MMITrain::getOutput(int output_id, int count)
{
   //cerr << "Getting output in MMITrain\n";
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         int i,j;
         NodeInput framesInput = inputs[framesInputID];

         ObjectRef matRef = framesInput.node->getOutput(framesInput.outputID,count);

         Vector<ObjectRef> &mat = object_cast<Vector<ObjectRef> > (matRef);

         int dimensions = object_cast<vector<float> >((object_cast <Vector<ObjectRef> > (mat[0])[0])).size();
         //cerr << "Dimensions = " << dimensions << endl;
         //cerr << "Number of Classes: " << mat.size() << endl;
         Cell *mmi = new Cell(dimensions, mat.size()); 
	 //cerr << "first cell created with dim " << dimensions << " size " 
	 //     << mat.size() << endl;
         vector <pair<int, float *> > data;
	 //cerr << "inserving\n";
         for (i=0;i< mat.size();i++)
         {
	    cerr << i << endl;
            Vector<ObjectRef>  &speaker = object_cast <Vector<ObjectRef> > (mat[i]);
	    //cerr << "class " << i << " has " << speaker.size() << " members\n";
            for (j=0;j<speaker.size(); j++)
            {
               data.insert (data.end(), 
                            make_pair<int, float *> (i, &object_cast <Vector<float> > (speaker[j])[0]));
            }
         }
         //cerr << "splitting...\n";
         mmi->recursiveSplit(data, nb_levels);
         mmi->setNumbering();
         currentMMI = ObjectRef(mmi);
         //exit(1);
      }
      return currentMMI;
   }
   else 
      throw new NodeException (this, "MMITrain: Unknown output id", __FILE__, __LINE__);
}
