// Copyright (C) 2000 Dominic Letourneau (doumdi@yahoo.com)

// GenericModel.cc: implementation of the GenericModel class.
//
//////////////////////////////////////////////////////////////////////

#include "GenericModel.h"
#include "Exception.h"


DECLARE_NODE(GenericModel)
DECLARE_TYPE(GenericModel)
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
 * @output_name MODEL
 * @output_description The model (cloned)
 * @output_type Model
 *
 * @output_name OUTPUT
 * @output_description The defuzzified values
 * @output_type Vector
 *
END*/


//////////////////////////////////////////////////////////////////////
//Construction
//////////////////////////////////////////////////////////////////////

GenericModel::GenericModel() {

  //cerr<<"Creating Generic Model"<<endl;
}

GenericModel::GenericModel(string nodeName, ParameterSet params) 
: FuzzyModel(nodeName,params){


}

GenericModel::GenericModel(const GenericModel &model)
  : FuzzyModel(model) {



}
//////////////////////////////////////////////////////////////////////
// Destruction
//////////////////////////////////////////////////////////////////////
GenericModel::~GenericModel() {

  //cerr<<"generic Model Destructor"<<endl;

}

//////////////////////////////////////////////////////////////////////
// Fuzzy conjunction operator (min)
//////////////////////////////////////////////////////////////////////
float GenericModel::conjunction(Vector<float> &c_values) {

  
  if (c_values.size() == 0) {
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
float GenericModel::disjunction(Vector<float> &d_values) {

  if (d_values.size() == 0) {
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


Vector<float>& GenericModel::defuzzification() {
  
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
    
    Vector<ObjectRef> & functions = object_cast<FuzzySet>(m_output_set[i]).get_member_functions();
    
    for (int j = 0;j < functions.size(); j++) {
      
      tmp_area = object_cast<FuzzyFunction>(functions[j]).get_area();
      tmp_cog = object_cast<FuzzyFunction>(functions[j]).get_center_of_gravity();
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



ObjectRef GenericModel::clone() {


  //cerr<<"cloning Generic Model"<<endl;

  GenericModel *model = new GenericModel(*this);

  return ObjectRef(model);

}

void GenericModel::printOn(ostream &out) const {

  out << "<GenericModel "<<endl; 

  //inputset
  for (int i = 0; i < m_input_set.size(); i++) {
    out<<"<InputSet "<<m_input_set[i]<<" >"<<endl;
  }
  
  //outputset
  for (int i = 0; i < m_output_set.size(); i++) {
    out<<"<OutputSet "<<m_output_set[i]<<" >"<<endl;
  }

  //rules
  for (int i = 0; i < m_rules.size(); i++) {
    out<<"<Rule "<<m_rules[i]<<" >"<<endl;    
  }

  out <<" >\n";
}
	
void GenericModel::readFrom(istream &in) {

   string tag;
   int rule_size;
   int input_set_size;
   int output_set_size;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;

      else if (ch != '<') {
       throw new ParsingException ("Parse error: '<' expected");
      }
      in >> tag;

      if (tag == "InputSet") {
	ObjectRef value;
	in>>value;
	add_fuzzy_set(value,FuzzyModel::FUZZY_INPUT_SET);
      }
      else if (tag == "OutputSet") {
	ObjectRef value;
	in>>value;
	add_fuzzy_set(value, FuzzyModel::FUZZY_OUTPUT_SET);
      }
      else if (tag == "Rule") {
	ObjectRef value;
	in>>value;
	add_fuzzy_rule(value);
      }
      else {
	throw new ParsingException ("unknown argument: " + tag);
      }

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }

}
