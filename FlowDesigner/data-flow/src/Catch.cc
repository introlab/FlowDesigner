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
#include "FlowException.h"

class Catch;

DECLARE_NODE(Catch)
/*Node
 *
 * @name Catch
 * @category Flow
 * @description Catches an exception
 *
 * @input_name INPUT
 * @input_description Normal flow
 *
 * @input_name CATCH
 * @input_description Flow to follow is an exception is caught
 *
 * @output_name OUTPUT
 * @output_description Flow output
 *
END*/


class Catch : public Node {
protected:
   int inputID;
   int catchID;
   int outputID;

public:
   Catch(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
	 inputID=addInput("INPUT");
	 catchID=addInput("CATCH");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in Catch constructor", __FILE__, __LINE__));
      }
      
   }

   ObjectRef getOutput(int output_id, int count)
   {
      try 
      {
	 ObjectRef inputValue = getInput(inputID, count);
	 return inputValue;
      } catch (Ptr<FlowException> e)
      {
	 return getInput(catchID, count);
      }
   }


};
