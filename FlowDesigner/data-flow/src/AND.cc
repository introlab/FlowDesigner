// Copyright (C) 1999 Dominic Letourneau
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

#ifndef _AND_CC_
#define _AND_CC_

#include "AND.h"
#include "Object.h"
#include "ObjectRef.h"
#include "Exception.h"

//DECLARE_NODE(AND)
NODE_INFO(AND,"Logic", "INPUT1:INPUT2", "OUTPUT", "")

AND::AND(string nodeName, ParameterSet params)
 
   : Node(nodeName, params)
   , output (new Bool(false)){

   addOutput("OUTPUT");
}

ObjectRef AND::getOutput (int output_id, int count) {
   
   int i;
   int true_count = 0;
   int false_count = 0;

   if (!hasOutput(output_id)) throw NodeException (this, "Cannot getOutput id",__FILE__,__LINE__);

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
         catch (GenericCastException &e) {
            //We had a problem casting, our inputs are invalid?
            e.print();
            false_count++;
         }         
         catch (BaseException &e) {
            //Something weird happened
            e.print();
            throw NodeException (this,string("Cannot get BOOL value from") + 
                                 inputs[i].node->getName()
                                 , __FILE__,__LINE__);
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


#endif
