// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>
#include <sstream>

class ConstantVector;

DECLARE_NODE(ConstantVector)
/*Node

 * @name ConstantVector
 * @category General
 * @description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name VALUE
 * @parameter_description No description available

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
