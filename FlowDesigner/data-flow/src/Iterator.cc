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

   //let's add our translator node
   translator = new InputTranslator("ITERATOR_TRANSLATOR",ParameterSet());  
   addNode (*translator);   

}
/***************************************************************************/
/*
  Iterator::getOutput(...)
  Dominic Letourneau
 */
/***************************************************************************/
ObjectRef Iterator::getOutput (int output_id, int count) {
   
   if (!hasOutput(output_id)) throw new NodeException (this, "Cannot getOutput id",__FILE__,__LINE__);

   if (processCount != count) {

      try {
         
         //Reinitialization of all the processCount in our Network
         map<string,Node*>::iterator iter;         
 
         for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) {
            (*iter).second->specificInitialize();
         }

         //We are doing a little trick for the translator (real inputNode)

         translator->setProcessCount(count);
         int conditionID = conditionNode->translateOutput("OUTPUT");

         for (int pc = 0;dereference_cast<bool>(conditionNode->getOutput(conditionID,pc)) == true; pc++) {
            //we are doing the iterations here
            output = sinkNode->getOutput(output_id,pc);
         }
      }
      catch (GenericCastException *e) {
         //We had a problem casting, our inputs are invalid?
         e->print();
         output = ObjectRef(new Object(Object::nil));         
      }      
      catch (BaseException *e) {
         //Something weird happened
         e->print();
         throw NodeException (this,string("Error!!! "), __FILE__,__LINE__);
      }      
   }

   return output;   
}

/***************************************************************************/
/*
  Iterator::connectToNode(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Iterator::connectToNode(unsigned int in, Node *inNode, unsigned int out) {
  
   int translator_out = ((Node*) translator)->translateOutput(inNode->outputNames[out]);

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
   
}


#endif
