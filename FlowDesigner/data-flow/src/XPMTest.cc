// Copyright (C) 2003 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include "test.xpm"

using namespace std;

class XPMTest;
DECLARE_NODE_XPM(XPMTest, test_xpm)
/*Node
 *
 * @name XPMTest
 * @category Test
 * @description XPM test
 *
 * @input_name INPUT1
 * @input_description The first input
 *
 * @input_name INPUT2
 * @input_description The second input
 *
 * @output_name OUTPUT1
 * @output_description The first output
 *
 * @output_name OUTPUT2
 * @output_description The second output
 *
END*/


class XPMTest : public BufferedNode {
protected:
  //outputs
  int output1ID;
  int output2ID;
  
  //inputs
  int input1ID;
  int input2ID;

public:
   ///Constructor, takes the name of the node and a set of parameters
   XPMTest(string nodeName, ParameterSet params) 
     : BufferedNode(nodeName, params)
   {
      input1ID = addInput ("INPUT1");
      input2ID = addInput ("INPUT2");
      output1ID = addOutput ("OUTPUT1"); 
      output2ID = addOutput ("OUTPUT2");
   }
   


  void calculate(int output_id, int count, Buffer &out) {
     out[count] = nilObject;
   }
   
};

