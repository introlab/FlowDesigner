// Copyright (C) 2001 Locus Dialog (author: Jean-Marc Valin)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


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
 * @output_type Vector
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
      
      val.resize(dereference_cast<int> (parameters.get("VALUE")), 
		 dereference_cast<float> (parameters.get("VALUE")));
   }

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