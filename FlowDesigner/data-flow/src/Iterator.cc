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

#ifndef _ITERATOR_CC_
#define _ITERATOR_CC_

#include "Iterator.h"
#include "Node.h"
#include "ObjectRef.h"

/***************************************************************************/
/*
  Iterator::Iterator(...)
  Dominic Letourneau
 */
/***************************************************************************/
Iterator::Iterator (string nodeName, ParameterSet params) 
   : Network(nodeName, params), output(new Object(Object::nil)) {
   translator = NULL;
}
/***************************************************************************/
/*
  Iterator::getOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
ObjectRef Iterator::getOutput (int output_id, int count) {
   
   if (!hasOutput(output_id)) throw NodeException (this, "Cannot getOutput id",__FILE__,__LINE__);

   lock();

   if (processCount != count) {

      try {
         
         //Reinitialization of all the processCount in our Network
         map<string,Node*>::iterator iter;         
 
         if (processCount != -1 ) {

            for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) {
               if (debugMode) {
                  cout<<"DEBUG : Iterator is now resetting node "<<(*iter).second->getName()<<endl;
               }
               (*iter).second->reset();
            }

         }
         //We are doing a little trick for the translator (real inputNode)
         //if it exists
         if (translator) {
            translator->setProcessCount(count);
         }

         int conditionID = conditionNode->translateOutput("OUTPUT");

         int pc = 0;
         while(1)
         {
            if (doWhile)
            {
               output = sinkNode->getOutput(output_id,pc);
            }
            ObjectRef condition = conditionNode->getOutput(conditionID,pc);
            
            if (dereference_cast<bool>(condition)==false) break;
            if (!doWhile)
            {
               output = sinkNode->getOutput(output_id,pc);
            }
            pc++;
         }
      }
      catch (GenericCastException &e) {
         //We had a problem casting, our inputs are invalid?
         e.print();
         output = ObjectRef(new Object(Object::nil));         
      }      
      catch (BaseException &e) {
         //Something weird happened
         e.print();
         throw NodeException (this,string("Error in Iterator::getOutput"), __FILE__,__LINE__);
      }
      processCount = count;
   }

   unlock();

   return output;   
}

/***************************************************************************/
/*
  Iterator::connectToNode(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Iterator::connectToNode(unsigned int in, Node *inNode, unsigned int out) {
 
   if (!inputNode) 
      throw NodeException(this,"Trying to connect without input node",__FILE__,__LINE__);

   if (!translator) {
      //let's add our translator node
      translator = new InputTranslator("ITERATOR_TRANSLATOR",ParameterSet());  
      addNode (*translator);   
   }

   int translator_out = translator->addInput(this->getInputs()[in].name);

   // Connecting the inputNode
   inputNode->connectToNode(in,translator,translator_out);

   // We are connecting the translator
   translator->connectToNode(translator_out,inNode,out);

}
/***************************************************************************/
/*
  Iterator::specificInitialize(...)
  Dominic Letourneau
 */
/***************************************************************************/   
void Iterator::specificInitialize() {

   if (!conditionNode) {
      throw NodeException(this,"No condition Node specified in Iterator",__FILE__,__LINE__);
   }

   // We must initialize the conditionNode before

   conditionNode->initialize();

   this->Network::specificInitialize();
   
   if (parameters.exist("DOWHILE"))
      doWhile = true;
   else 
      doWhile = false;
}


#endif
