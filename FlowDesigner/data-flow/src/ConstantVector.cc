// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>
#include <sstream>

class ConstantVector;

DECLARE_NODE(ConstantVector)
/*Node
 *
 * @name ConstantVector
 * @category General
 * @description Creates a Constant vector
 *
 * @output_name OUTPUT
 * @output_description The vector
 * @output_type Vector
 *
 * @parameter_name VALUE
 * @parameter_description The string representation of the vector
 * @parameter_type string
 *
END*/


/** A constant node contains a value that will never changes. */
class ConstantVector : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /**The ID of the 'value' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   ConstantVector(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      //, value (parameters.get("VALUE"))
   {
      outputID = addOutput("OUTPUT");
      
      value = ObjectRef(new Vector<float>);
      Vector<float> &val = object_cast<Vector<float> > (value);
      //istrstream str_vector(object_cast <String> (parameters.get("VALUE")).c_str());
      //str_vector >> val;
      istringstream str_vector(object_cast <String> (parameters.get("VALUE")).c_str());
      str_vector >> val;

      //cerr << "vector is: " << val << endl;
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw new NodeException (this, "ConstantVector: Unknown output id", __FILE__, __LINE__);
   }

protected:
   /**Default constructor, should not be used*/
   ConstantVector() {throw new GeneralException("ConstantVector copy constructor should not be called",__FILE__,__LINE__);}

};
