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
 * @output_name EXCEPTION
 * @output_description The exception caught (use only as feedback link)
 *
END*/


class Catch : public Node {
protected:
   int inputID;
   int catchID;
   int outputID;
   int exceptionID;
   bool isInside;
   ObjectRef currentException;

public:
   Catch(string nodeName, ParameterSet params)
      : Node(nodeName, params)
      , isInside(false)
   {
      try {
	 inputID=addInput("INPUT");
	 catchID=addInput("CATCH");
	 outputID=addOutput("OUTPUT");
	 exceptionID=addOutput("EXCEPTION");
      } catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in Catch constructor", __FILE__, __LINE__));
      }
      
   }

   /*WARNING: Do not try this at home. Overriding the registerOutput() method should not be 
              done unless you REALLY know what you're doing... and I'm not even sure 
              I know what I'm doing here*/
   void registerOutput (int out) 
   {
      if (out == outputID)
	 incrementOutputInitialize();
   }


   ObjectRef getOutput(int output_id, int count)
   {
      if (output_id == outputID)
      {
	 if (isInside)
	 {
	    cerr << "What the heck is going on??? " << endl;
	    throw NodeException (this, "I don't know what I'm doing", __FILE__, __LINE__);
	 }
	 try 
	 {
	    ObjectRef inputValue = getInput(inputID, count);
	    return inputValue;
	 } catch (Ptr<FlowException> e)
	 {
	    isInside = true;
	    currentException = e->getObject();
	    ObjectRef catchValue(NULL);
	    try 
	    {
	       catchValue = getInput(catchID, count);
	    } catch (...)
	    {
	       throw;
	       //cerr << "The catch flow is throwing an exception... I'm confused" << endl;
	       //throw NodeException (this, "The catch flow is throwing an exception", __FILE__, __LINE__);
	    }
	    isInside = false;
	    return catchValue;
	 }
      } else if (output_id==exceptionID) 
      {
	 if (!isInside)
	 {
	    throw NodeException (this, "The EXCEPTION output is only for the catch flow", __FILE__, __LINE__);
	 }
	 return currentException;
      } else {
	 throw NodeException (this, "Output not found", __FILE__, __LINE__);
      }
   }


};
