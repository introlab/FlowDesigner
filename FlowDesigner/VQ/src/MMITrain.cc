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

#include "MMITrain.h"
#include "net_types.h"
#include "Buffer.h"
#include "Cell.h"
#include "Vector.h"

//DECLARE_NODE(MMITrain)
NODE_INFO(MMITrain,"VQ", "FRAMES", "OUTPUT", "")

MMITrain::MMITrain(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   //cerr << "MMITrain initialize\n";
   outputID = addOutput("OUTPUT");
   framesInputID = addInput("FRAMES");
}

void MMITrain::specificInitialize()
{
   this->Node::specificInitialize();
}

void MMITrain::reset()
{
   this->Node::reset();
}

ObjectRef MMITrain::getOutput(int output_id, int count)
{
   //cerr << "Getting output in MMITrain\n";
   if (output_id==outputID)
   {
      lock();
      if (count != processCount)
      {
         int i,j;
         NodeInput framesInput = inputs[framesInputID];

         ObjectRef matRef = framesInput.node->getOutput(framesInput.outputID,count);

         Vector<ObjectRef> &mat = object_cast<Vector<ObjectRef> > (matRef);

         int dimensions = object_cast<vector<float> >((object_cast <Buffer> (mat[0])[0])).size();
         cerr << "Dimensions = " << dimensions << endl;
         cerr << "Number of Classes: " << mat.size() << endl;
         Cell *mmi = new Cell(dimensions, mat.size()); 

         vector <pair<int, float *> > data;
         for (i=0;i< mat.size();i++)
         {
            Buffer &speaker = object_cast <Buffer> (mat[i]);
            for (j=0;j<speaker.getCurrentPos(); j++)
            {
               data.insert (data.end(), 
                            make_pair<int, float *> (i, object_cast <Vector<float> > (speaker[j]).begin()));
            }
         }
         
         mmi->recursiveSplit(data, 10);
         mmi->setNumbering();
         currentMMI = ObjectRef(mmi);
      }
      return unlock_and_return(currentMMI);
   }
   else 
      throw NodeException (this, "MMITrain: Unknown output id", __FILE__, __LINE__);
}
