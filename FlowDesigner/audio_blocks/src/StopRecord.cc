// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "vec.h"

class StopRecord;

DECLARE_NODE(StopRecord)
/*Node
 *
 * @name StopRecord
 * @category DSP:Misc
 * @description For SV
 *
 * @input_name INPUT
 * @input_type bool
 * @input_description frame by frame
 *
 * @output_name OUTPUT
 * @output_type bool
 * @output_description false when should stop
 *
 * @parameter_name TIMEOUT
 * @parameter_type int
 * @parameter_description Number of false frames
 *
END*/


class StopRecord : public BufferedNode {
   
   int inputID;
   int outputID;
   int timeout;
   int nbFalse;      
   bool decision;

public:
   StopRecord(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , nbFalse(0)
      , decision(true)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      timeout = dereference_cast<int> (parameters.get("TIMEOUT"));
      inOrder = true;
   }

   void reset()
   {
      decision=true;
      nbFalse=0;
      BufferedNode::reset();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      bool val = dereference_cast<bool> (inputValue);
      if (!val)
	 nbFalse++;
      else
	 nbFalse=0;

      if (nbFalse>=timeout)
	 decision=false;

      if (decision)
	 out[count] = TrueObject;
      else
	 out[count] = FalseObject;

      
   }

      
};
