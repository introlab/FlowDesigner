// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "Iterator.h"
#include "Node.h"
#include "ObjectRef.h"

//@implements core

/***************************************************************************/
/*
  Iterator::Iterator(...)
  Dominic Letourneau
 */
/***************************************************************************/
Iterator::Iterator (string nodeName, ParameterSet params) 
   : Network(nodeName, params)
   , exit_status(false) 

{
   translator = NULL;
   conditionNode = NULL;
   
   //FIXME: this should be set dynamically.
   output.resize(30);
}

void Iterator::cleanupNotify() {

   exit_status = true;
   Network::cleanupNotify();
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
 
         if (processCount != -1) {

            for (iter = nodeDictionary.begin(); iter != nodeDictionary.end(); iter++) {
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
         while(!exit_status)
         {
            if (doWhile)
            {
	       int out_id=0;
	       while (sinkNode->hasOutput(out_id))
	       {
		  output[out_id] = sinkNode->getOutput(out_id,pc);
		  out_id++;
	       }
            }
            ObjectRef condition = conditionNode->getOutput(conditionID,pc);
            
            if (dereference_cast<bool>(condition)==false) break;
            if (!doWhile)
            {
	       int out_id=0;
	       while (sinkNode->hasOutput(out_id))
	       {
		  output[out_id] = sinkNode->getOutput(out_id,pc);
		  out_id++;
	       }
	    }
            pc++;
         }
      }
      catch (BaseException *e) {
         //Something weird happened
         //e->print();
         throw e->add (new NodeException (this,string("Error in Iterator::getOutput"), __FILE__,__LINE__));
      }
      processCount = count;
      }


   return output[output_id];   
}

/***************************************************************************/
/*
  Iterator::connectToNode(...)
  Dominic Letourneau
 */
/***************************************************************************/
void Iterator::connectToNode(unsigned int in, Node *inNode, unsigned int out) {
 
   if (!inputNode) 
      throw new NodeException(this,"Trying to connect without input node",__FILE__,__LINE__);

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
      throw new NodeException(this,"No condition Node specified in Iterator",__FILE__,__LINE__);
   }

   // We must initialize the conditionNode before

   conditionNode->initialize();

   this->Network::specificInitialize();
   
   if (parameters.exist("DOWHILE"))
      doWhile = true;
   else 
      doWhile = false;
   
   processCount = -1;
}

void Iterator::reset()
{
   processCount = -1;
   Network::reset();
}
