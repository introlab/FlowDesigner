// Copyright (C) 1999 Dominic Letourneau

#include "NOT.h"
#include "Object.h"
#include "ObjectRef.h"
#include "Exception.h"

DECLARE_NODE(NOT)
/*Node
 *
 * @name NOT
 * @category Logic
 * @description Binary NOT
 *
 * @input_name INPUT
 * @input_description The Bool Input
 * @input_type bool
 *
 * @output_name OUTPUT
 * @output_description The Inverted Bool Output
 * @output_type bool
 *
END*/


NOT::NOT(string nodeName, ParameterSet params)
 
   : Node(nodeName, params)
   , output (new Bool(false)){

   outputID = addOutput("OUTPUT");
   inputID = addInput ("INPUT");
}

ObjectRef NOT::getOutput (int output_id, int count) {
   
 
   if (!hasOutput(output_id)) throw new NodeException (this, "Cannot getOutput id",__FILE__,__LINE__);

   if (count != processCount) {
      //We are updating our output only if needed
      
      try {
         
         //getting all data from our inputs.
         int OutputID = inputs[inputID].outputID;
         
         bool value = dereference_cast<bool> (inputs[inputID].node->getOutput(OutputID, count));
         
         //updating our output

         if (value == true) {
            output = ObjectRef(new Bool(false));
         }
         else {
            output = ObjectRef(new Bool(true));
         }
         
         //updating processCount
         processCount = count;                          
         
      } //end of try block
      
      catch (GenericCastException *e) {
         //We had a problem casting, our inputs are invalid?
         e->print();
         output = ObjectRef(new Object(Object::nil));
      }         
      catch (BaseException *e) {
         //Something weird happened
         //e->print();
         throw e->add(new NodeException (this,string("Cannot get BOOL value from") + 
                              inputs[inputID].node->getName()
                              , __FILE__,__LINE__));
      }
   }
   
   return output;   
}

