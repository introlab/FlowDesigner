// Copyright (C) 1999 Dominic Letourneau

#include "AND.h"
#include "Object.h"
#include "ObjectRef.h"
#include "Exception.h"

DECLARE_NODE(AND)
/*Node
 *
 * @name AND
 * @category Logic
 * @description Logical AND between two inputs
 *
 * @input_name INPUT1
 * @input_type bool
 * @input_description First boolean input
 *
 * @input_name INPUT2
 * @input_type bool
 * @input_description Second boolean input
 *
 * @output_name OUTPUT
 * @output_type bool
 * @output_description Boolean output
 *
END*/


AND::AND(string nodeName, ParameterSet params)
 
   : Node(nodeName, params)
   , output (new Bool(false)){

   addOutput("OUTPUT");
}

ObjectRef AND::getOutput (int output_id, int count) {
   
   int i;
   int true_count = 0;
   int false_count = 0;

   if (!hasOutput(output_id)) throw new NodeException (this, "Cannot getOutput id",__FILE__,__LINE__);

   if (count != processCount) {
      //We are updating our output only if needed
      
      for (i = 0; i< inputs.size(); i++) {
         try {
            
            //getting all data from our inputs.
            int OutputID = inputs[i].outputID;
            
            bool value = dereference_cast<bool> (inputs[i].node->getOutput(OutputID, count));
            
            if (value == true) {true_count++;}
            else {false_count++;}
            
         } //end of try block
         catch (GenericCastException *e) {
            //We had a problem casting, our inputs are invalid?
            e->print();
            false_count++;
         }         
         catch (BaseException *e) {
            //Something weird happened
            //e->print();
            throw e->add(new NodeException (this,string("Cannot get BOOL value from") + 
                                 inputs[i].node->getName()
                                 , __FILE__,__LINE__));
         }      
      } //end of for
      
      
      //updating processCount
      processCount = count;                 
      
      if (true_count > 0 && false_count == 0) {
         output = ObjectRef (new Bool(true));
      }
      else {
         output = ObjectRef (new Bool(false));
      }

   }
   
   return output;
 
}

int AND::translateInput(string inputName) {
   // just adding the input to the OR */
   return addInput(inputName);
}

