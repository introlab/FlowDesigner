// Copyright (C) 1999 Jean-Marc Valin

#include "MMIScore.h"
#include "net_types.h"
#include "Buffer.h"
#include "Cell.h"
#include "Vector.h"

using namespace std;

namespace FD {

DECLARE_NODE(MMIScore)
/*Node
 *
 * @name MMIScore
 * @category VQ
 * @description No description available
 *
 * @input_name FRAMES
 * @input_description No description available
 *
 * @input_name MMI
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


MMIScore::MMIScore(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   mmiInputID = addInput("MMI");
   framesInputID = addInput("FRAMES");
}

void MMIScore::initialize()
{
   processCount=-1;
   this->Node::initialize();
}

void MMIScore::reset()
{
   processCount=-1;
   this->Node::reset();
}

ObjectRef MMIScore::getOutput(int output_id, int count)
{
   //cerr << "Getting output in MMIScore with count = " << count << endl;
   if (output_id==outputID)
   {
      if (count != processCount)
      {

         NodeInput framesInput = inputs[framesInputID];
         NodeInput mmiInput = inputs[mmiInputID];
         
         ObjectRef inputValue = framesInput.node->getOutput(framesInput.outputID,count);
         //FUTURE: return the right "empty object" instead of inputValue (for buffer reasons)
         Vector<float> &inputFrame = object_cast<Vector<float> > (inputValue);
         
         Cell &mmi = object_cast<Cell> (mmiInput.node->getOutput(mmiInput.outputID,count));

         int cellID = mmi.belongs(&inputFrame[0]);
         //cerr << "Cell: " << cellID << endl;
         currentScore = ObjectRef(Int::alloc(cellID));
         processCount=count;
      }
      //cerr << "MMIScore returning: " << currentScore << " (" << typeid(currentScore).name() << ")" << endl;
      return currentScore;
   }
   else 
      throw new NodeException (this, "MMIScore: Unknown output id", __FILE__, __LINE__);
}
}//namespace FD
