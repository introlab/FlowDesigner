// Copyright (C) 2001 Jean-Marc Valin

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
 * @input_description Flow to follow if an exception is caught
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

   /**Propagate requests*/
   virtual void request(int outID, const ParameterSet &req)
   {
      if (outID==outputID)
      {
         inputs[inputID].node->request(inputs[inputID].outputID, req);
         inputs[catchID].node->request(inputs[catchID].outputID, req);
      }
   }


   ObjectRef getOutput(int output_id, int count)
   {
      if (output_id == outputID)
      {
	 if (isInside)
	 {
	    cerr << "What the heck is going on??? " << endl;
	    throw new NodeException (this, "I don't know what I'm doing", __FILE__, __LINE__);
	 }
	 try 
	 {
	    ObjectRef inputValue = getInput(inputID, count);
	    return inputValue;
	 } catch (RCPtr<FlowException> e)
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
	    throw new NodeException (this, "The EXCEPTION output is only for the catch flow", __FILE__, __LINE__);
	 }
	 return currentException;
      } else {
	 throw new NodeException (this, "Output not found", __FILE__, __LINE__);
      }
   }


};
