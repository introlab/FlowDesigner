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
 * @parameter_name START
 * @parameter_type int
 * @parameter_value 1
 * @parameter_description Number of true frames before start
 *
 * @parameter_name TIMEOUT
 * @parameter_type int
 * @parameter_description Number of false frames before end
 *
END*/


class StopRecord : public BufferedNode {
   
   int inputID;
   int outputID;
   int timeout;
   int nbFalse;      
   bool decision;
   int nbStart;
   int start;

public:
   StopRecord(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , nbFalse(0)
      , decision(true)
      , start(0)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      timeout = dereference_cast<int> (parameters.get("TIMEOUT"));
      if (parameters.exist("START"))
	 nbStart=dereference_cast<int> (parameters.get("START"));
      else
	 nbStart=1;

      inOrder = true;
   }

   void reset()
   {
      decision=true;
      start=0;
      nbFalse=0;
      BufferedNode::reset();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      bool val = dereference_cast<bool> (inputValue);
      if (val)
      {
	 start++;
	 nbFalse=0;
      } else
      {
	 if (start>=nbStart)
	    nbFalse++;
      }

      if (nbFalse>=timeout)
	 decision=false;

      if (decision)
	 out[count] = TrueObject;
      else
	 out[count] = FalseObject;

      
   }

      
};
