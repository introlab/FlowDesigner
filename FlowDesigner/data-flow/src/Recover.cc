// Copyright (C) 2001 Jean-Marc Valin

#include "Node.h"
#include <sstream>

class Recover;

DECLARE_NODE(Recover)
/*Node
 *
 * @name Recover
 * @category Flow
 * @description Recovers from an error (BaseException)
 *
 * @input_name INPUT
 * @input_description Normal flow
 *
 * @input_name CATCH
 * @input_description Flow to follow is an error (exception) is caught
 *
 * @output_name OUTPUT
 * @output_description Flow output
 *
 * @output_name EXCEPTION
 * @output_type String
 * @output_description The error message caught (use only as feedback link)
 *
END*/


class Recover : public Node {
protected:
   int inputID;
   int catchID;
   int outputID;
   int exceptionID;
   bool isInside;
   ObjectRef currentException;

public:
   Recover(string nodeName, ParameterSet params)
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
         throw e->add(new NodeException (NULL, "Exception caught in Recover constructor", __FILE__, __LINE__));
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
	    throw new NodeException (this, "I don't know what I'm doing", __FILE__, __LINE__);
	 }
	 try 
	 {
	    ObjectRef inputValue = getInput(inputID, count);
	    return inputValue;
	 } catch (BaseException *e)
	 {
	    isInside = true;
	    ostringstream sstr;	    
	    e->print(sstr);
	    currentException = ObjectRef(new String(sstr.str()));
	    ObjectRef catchValue(NULL);
	    try 
	    {
	       catchValue = getInput(catchID, count);
	    } catch (...)
	    {
	       throw;
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
