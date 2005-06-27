// Copyright (C) 2002 Jean-Marc Valin

#include "BufferedNode.h"
#include "CompositeType.h"
#include "Exception.h"
#include "BaseException.h"

using namespace std;

namespace FD {

class CompositeEmpty;

DECLARE_NODE(CompositeEmpty)
/*Node
 *
 * @name CompositeEmpty
 * @category General
 * @description Verify if a CompositeType is empty (no fields)
 *
 * @input_name INPUT
 * @input_type CompositeType
 * @input_description The CompositeType to verify.
 *
 * @output_name OUTPUT
 * @output_description True if a CompositeType is empty.
 * @output_type bool
 *
END*/

class CompositeEmpty : public BufferedNode {
   
  int m_inputID;
  int m_outputID;

public:
   CompositeEmpty(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
     m_inputID = addInput("INPUT");
     m_outputID = addOutput("OUTPUT");
   }

  void calculate(int output_id, int count, Buffer &out)
   {
     try {
       RCPtr<CompositeType> inputComposite = getInput(m_inputID,count);

       CompositeType::map_type fields = inputComposite->getAllFields();

       if (fields.empty()) {
	 out[count] = ObjectRef(Bool::alloc(true));
       }
       else {
	 out[count] = ObjectRef(Bool::alloc(false));
       }

     }
     catch(BaseException *e) {
       throw e->add(new GeneralException("Expected a CompositeType as input",__FILE__,__LINE__));
     }
   }

   NO_ORDER_NODE_SPEEDUP(CompositeEmpty)
      
};

}//namespace FD
