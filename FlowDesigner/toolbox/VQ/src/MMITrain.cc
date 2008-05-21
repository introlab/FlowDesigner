// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "net_types.h"
#include "Cell.h"
#include "Vector.h"

using namespace std;

namespace FD {

class MMITrain;
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


class MMITrain : public BufferedNode {

protected:
   
   int framesInputID;
   int outputID;

   int nb_levels;
      
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   MMITrain(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      framesInputID = addInput("FRAMES");
      nb_levels=dereference_cast<int> (parameters.get("LEVELS"));
   }
      

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
   {
      int i,j;
      NodeInput framesInput = inputs[framesInputID];
      
      ObjectRef matRef = getInput(framesInputID,count);
      
      Vector<ObjectRef> &mat = object_cast<Vector<ObjectRef> > (matRef);
      
      int dimensions = object_cast<Vector<float> >((object_cast <Vector<ObjectRef> > (mat[0])[0])).size();
      Cell *mmi = new Cell(dimensions, mat.size()); 
      vector <pair<int, float *> > data;
      for (i=0;i< mat.size();i++)
      {
         cerr << i << endl;
         Vector<ObjectRef>  &speaker = object_cast <Vector<ObjectRef> > (mat[i]);
         for (j=0;j<speaker.size(); j++)
         {
            data.insert (data.end(), 
                         make_pair<int, float *> (i, &object_cast <Vector<float> > (speaker[j])[0]));
         }
      }
      mmi->recursiveSplit(data, nb_levels);
      mmi->setNumbering();
      out[count] = ObjectRef(mmi);
   }

};
}//namespace FD
