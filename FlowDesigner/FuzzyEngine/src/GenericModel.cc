// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// GenericModel.cc: implementation of the GenericModel class.
//
//////////////////////////////////////////////////////////////////////

#include "GenericModel.h"
#include "Exception.h"


DECLARE_NODE(GenericModel)
/*Node
 *
 * @name GenericModel
 * @category Fuzzy
 * @description A generic Fuzzy controller
 *
 * @input_name RULES
 * @input_description The Rules to use
 * @input_type Vector
 *
 * @input_name ANTECEDENT_SETS
 * @input_description The Sets to use
 * @input_type Vector
 *
 * @input_name CONSEQUENT_SETS
 * @input_description The Sets to use
 * @input_type Vecto
 * 
 * @input_name INPUT
 * @input_description The input value of the variables
 * @input_type Vector
 *
 * @output_name OUTPUT
 * @output_description The defuzzified values
 * @output_type Vector
 *
END*/


//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////

GenericModel::GenericModel() {

}

GenericModel::GenericModel(string nodeName, ParameterSet params) 
: FuzzyModel(nodeName,params){


}


//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////
GenericModel::~GenericModel() {

}

//////////////////////////////////////////////////////////////////////
// Fuzzy conjunction operator (min)
//////////////////////////////////////////////////////////////////////
float GenericModel::conjunction(vector<float> &c_values) {

  
  if (c_values.empty()) {
    throw new GeneralException("Conjunction values vector empty",__FILE__,__LINE__);
  }
  
  float value = c_values[0];
  
  //min operator
  for (int i = 0; i < c_values.size(); i++) {
    if (c_values[i] < value) {
      value = c_values[i];
    }
  }
  
  
  return value;
}
//////////////////////////////////////////////////////////////////////
// Fuzzy disjunction operator (max)
//////////////////////////////////////////////////////////////////////
float GenericModel::disjunction(vector<float> &d_values) {

  if (d_values.empty()) {
    throw new GeneralException("Disjunction values vector empty",__FILE__,__LINE__);
  }
  
  float value = d_values[0];
  
  //max operator
  for (int i = 0; i < d_values.size(); i++) {
    
    if (d_values[i] > value) {
      value = d_values[i];
    }
  }
  
  return value;
}


vector<float>& GenericModel::defuzzification() {
  
  m_defuzzification.resize(0);
  
  float defuzz_value;
  float area_cumul;
  float tmp_area;
  float tmp_cog;
  
  //for every output set we will calculate the defuzzified value
  //using Larsen (product) and the centroid method
  
  
  for (int i = 0; i < m_output_set.size(); i++) {
    
    area_cumul = 0;
    defuzz_value = 0;
    
    vector<FuzzyFunction*> & functions = m_output_set[i]->get_member_functions();
    
    for (int j = 0;j < functions.size(); j++) {
      
      tmp_area = functions[j]->get_area();
      tmp_cog = functions[j]->get_center_of_gravity();
      area_cumul += tmp_area;
      defuzz_value += tmp_area * tmp_cog;
    }//for every member function
    
    if (area_cumul != 0) {
      defuzz_value /= area_cumul;
    }
    //adding this output
    m_defuzzification.push_back(defuzz_value);
    
  }//for every set
  
  
  return m_defuzzification;
}
//////////////////////////////////////////////////////////////////////
// calculate
//////////////////////////////////////////////////////////////////////

void GenericModel::calculate(int output_id, int count, Buffer &out) {


  for (int i = 0 ; i < m_rules.size(); i++) {
    delete m_rules[i];
  }
  m_rules.resize(0);


  for (int i = 0 ; i < m_output_set.size(); i++) {
    delete m_output_set[i];
  }
  m_output_set.resize(0);

  
  for (int i = 0 ; i < m_input_set.size(); i++) {
    delete m_input_set[i];
  }
  m_input_set.resize(0);
  

  //getting Fuzzy Rules
  ObjectRef Rules = getInput(m_RuleID, count);

  //getting Fuzzy Sets (antecedent)
  ObjectRef ASets = getInput(m_ASetID, count);

  //getting Fuzzy Sets (consequent)
  ObjectRef CSets = getInput(m_CSetID,count);
  
  //getting Inputs
  ObjectRef Input = getInput(m_InputID, count);


  //First add antecedant sets
  Vector<FuzzySet*> &vect_sets = object_cast<Vector<FuzzySet*> >(ASets);
  for (int i = 0; i < vect_sets.size(); i++) {
    add_fuzzy_set(vect_sets[i],FuzzyModel::FUZZY_INPUT_SET);
  }

  //Then add consequent sets
  vect_sets = object_cast<Vector<FuzzySet*> >(CSets);
  for (int i = 0; i < vect_sets.size(); i++) {
    add_fuzzy_set(vect_sets[i],FuzzyModel::FUZZY_OUTPUT_SET);
  }
  
  //Finally add rules
  Vector<FuzzyRule*> &vect_rules = object_cast<Vector<FuzzyRule*> >(Rules);
  for (int i = 0; i < vect_rules.size(); i++) {
    add_fuzzy_rule(vect_rules[i]);
  }

  //verify rule consistency
  verify_rules();
  cerr<<"rules verified"<<endl;

  //calculate output
  Vector<float> &vect_value = object_cast<Vector<float> >(Input);  

  for (int i = 0; i <   vect_value.size(); i++) {
    cerr<<"got input : "<<vect_value[i]<<endl;
  }

  vector<float>& calc_output = evaluate(vect_value);


  Vector<float> *my_output = new Vector<float>(calc_output.size());

  for (int i = 0; i < calc_output.size(); i++) {
    (*my_output)[i] = calc_output[i];
  }

  out[count] = ObjectRef(my_output);

}
