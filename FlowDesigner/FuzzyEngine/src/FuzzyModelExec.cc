// Copyright (C) 2001 Dominic Letourneau (doumdi@yahoo.com)
//
//////////////////////////////////////////////////////////////////////


#include "BufferedNode.h"
#include "Buffer.h"
#include "FuzzyOperators.h"
#include "GenericModel.h"

class FuzzyModelExec;

DECLARE_NODE(FuzzyModelExec)
/*Node
 *
 * @name FuzzyModelExec
 * @category Fuzzy
 * @description FuzzyModelExec takes a FuzzyModel and find its output according the the specified intput.
 *
 * @input_name MODEL
 * @input_description The model to use
 *
 * @input_name INPUT
 * @input_description The input values to calculate
 *
 * @output_name OUTPUT
 * @output_description The output of the fuzzy model
 *
END*/


class FuzzyModelExec : public BufferedNode {
   
  int modelID;
  int inputID;
  int outputID;

public:

  FuzzyModelExec(string nodeName, ParameterSet params)
    : BufferedNode(nodeName, params) {
    
    inputID = addInput("INPUT");
    modelID = addInput("MODEL");
    outputID = addOutput("OUTPUT");
  }
  
   void calculate(int output_id, int count, Buffer &out) {

     try {

       ObjectRef modelRef = getInput(modelID,count);
       ObjectRef inputRef = getInput(inputID,count);
       
       FuzzyModel &model = object_cast<FuzzyModel>(modelRef);
       Vector<float> &input_value = object_cast<Vector<float> >(inputRef);  


       //we are now ready to calculate
       vector<float>& calc_output = model.evaluate(input_value);
       
       Vector<float> *my_output = new Vector<float>(calc_output.size());
       
       //copying output values
       for (int i = 0; i < calc_output.size(); i++) {
	 (*my_output)[i] = calc_output[i];
       }
       
       out[count] = ObjectRef(my_output);

       
     }//try
     catch (BaseException *e) {
       throw e->add (new GeneralException("Exception caught in FuzzyModelExec", __FILE__, __LINE__));
     }
     


   }//calculate

};
