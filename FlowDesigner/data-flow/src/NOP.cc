// Copyright (C) 1999 Jean-Marc Valin
//
// Modified 2003 Dominic Letourneau
// Changed to a BufferedNode.
//

#include "BufferedNode.h"

using namespace std;
using namespace FD;

class NOP;

DECLARE_NODE(NOP)
/*Node
 *
 * @name NOP
 * @category General
 * @description Pass Through (no operation)
 *
 * @input_name INPUT
 * @input_description The input
 *
 * @output_name OUTPUT
 * @output_description The output = The input
 *
END*/


class NOP : public BufferedNode {

protected:
   int inputID;
   int outputID;

public:


  NOP(string nodeName, ParameterSet params)
    : BufferedNode(nodeName, params) {
    try {
      inputID = addInput("INPUT");
      outputID= addOutput("OUTPUT");
    } 
    catch (BaseException *e) {
      throw e->add(new NodeException (NULL, "Exception caught in NOP constructor", __FILE__, __LINE__));
    } 
  }

  void calculate(int output_id, int count, Buffer &out) {

    ObjectRef input1Value = getInput(inputID, count);

    out[count] = input1Value;

  }
};
