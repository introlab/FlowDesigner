// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Collector.h"

//@implements core

using namespace std;
using namespace FD;

DECLARE_NODE(Collector)

/*Node

 * @name Collector
 * @category Flow
 * @description Pass through with unlimited number of input/output pairs

 * @input_name INPUT
 * @input_description The input

 * @output_name OUTPUT
 * @output_description The output = The input (same name)

END*/


Collector::Collector(string nodeName, ParameterSet params) 
   : Node(nodeName, params) 
{
}

ObjectRef Collector::getOutputNamed (const string &outputName, int count) {
   
   for (unsigned int i=0; i< inputs.size(); i++) {
      if (inputs[i].name == outputName) {
         return getOutput(i,count);
      }
   }   
 
   throw new NodeException(this,string("Unknown output name :")+outputName,__FILE__,__LINE__);
}


void Collector::initialize()
{
   this->Node::initialize();
}

int Collector::translateInput (string inputName)
{

   for (unsigned int i=0; i< inputs.size(); i++) {
      if (inputs[i].name == inputName) {
         return i;
      }
   }    

   return addInput(inputName);
}

int Collector::translateOutput (string outputName)
{
  // Simply call translateInput because it should return
  // the same integer...
  return translateInput(outputName);
}

bool Collector::hasOutput(int output_id) const
{
  return(int(inputs.size()) > output_id);
}

ObjectRef Collector::getOutput(int output_id, int count)
{
   if (hasOutput(output_id)) {
      try{	
	 int outputID = inputs[output_id].outputID;
	 return (inputs[output_id].node)->getOutput(outputID,count);
      } catch (BaseException *e)
      {
	 //e->print();
	 throw e->add(new NodeException (this, "Exception caught in Collector::getOutput", __FILE__, __LINE__));
      }
   }
   else {
      throw new NodeException(this,"Unknown output_id",__FILE__,__LINE__);
   }
}




