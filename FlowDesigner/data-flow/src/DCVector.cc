//Copyright (C) 2001 Locus Dialog 
//Author: Jean-Marc Valin


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>
#include <sstream>

class DCVector;

DECLARE_NODE(DCVector)
/*Node
 *
 * @name DCVector
 * @category Vector
 * @description Creates a vector of identical values
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description The vector
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description The vector length
 *
 * @parameter_name VALUE
 * @parameter_type float
 * @parameter_description Value of each element
 *
END*/


/** A constant node contains a value that will never changes. */
class DCVector : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /**The ID of the 'value' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   DCVector(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      //, value (parameters.get("VALUE"))
   {
      outputID = addOutput("OUTPUT");
      
      value = ObjectRef(new Vector<float>);
      Vector<float> &val = object_cast<Vector<float> > (value);
      
      val.resize(dereference_cast<int> (parameters.get("LENGTH")), 
		 dereference_cast<float> (parameters.get("VALUE")));
   }

   /**Do nothing for requests since we have no inputs*/
   virtual void request(int outputID, const ParameterSet &req) {}

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw new NodeException (this, "DCVector: Unknown output id", __FILE__, __LINE__);
   }

protected:
   /**Default constructor, should not be used*/
   DCVector() {throw new GeneralException("DCVector copy constructor should not be called",__FILE__,__LINE__);}

};
