// Copyright (C) 2001 Jean-Marc Valin

#include "BufferedNode.h"
#include "gmm.h"
#include "DiagGMM.h"

using namespace std;
using namespace FD;

class MakeDiagGMM;

DECLARE_NODE(MakeDiagGMM)
/*Node
 *
 * @name MakeDiagGMM
 * @category HMM
 * @require GMM
 * @description Transforms a GMM into a DiagGMM
 *
 * @input_name INPUT
 * @input_type GMM
 * @input_description Input GMM
 *
 * @output_name OUTPUT
 * @output_type DiagGMM
 * @output_description Output DiagGMM
 *
END*/


class MakeDiagGMM : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   MakeDiagGMM(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      
      GMM &gmm = object_cast<GMM> (inputValue);
      out[count] = ObjectRef(gmm.createDiagGMM());
      
   }

      
};
