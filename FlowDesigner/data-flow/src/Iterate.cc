// Copyright (C) 1999 Jean-Marc Valin
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

class Iterate;

DECLARE_NODE(Iterate)
/*Node

 * @name Iterate
 * @category Flow
 * @description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name MAX_ITER
 * @parameter_description No description available

END*/


class Iterate : public Node {
protected:
   ///The ID of the 'output' output
   int outputID;

   int maxIter;
   ObjectRef trueObject;
   ObjectRef falseObject;
public:
   ///Constructor, takes the name of the node and a set of parameters
   Iterate(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      outputID = addOutput ("OUTPUT");
      trueObject = ObjectRef (new Bool(true));
      falseObject = ObjectRef (new Bool(false));
      maxIter = dereference_cast<int> (parameters.get("MAX_ITER"));
   }
   
   virtual ObjectRef getOutput(int output_id, int count) 
   {
      if (count < maxIter)
	 return trueObject;
      else
	 return falseObject;
   }
   
};
