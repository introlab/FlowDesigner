// Copyright (C) 2001 Jean-Marc Valin

#include "UINodeParameters.h"
#include <libxml/tree.h>
#include "Node.h"
#include "UINode.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include "object_param.h"
#include "ParameterSet.h"

//@implements UIClasses

using namespace std;

namespace FD {

class ParamTypeChange {
public:
   char *newParam;
   string &value;
   
};

UINodeParameters::UINodeParameters(UINode *_node, string type)
   : node(_node)
{
   vector<ItemInfo *> tmp = node->getNetwork()->getDocument()->getNetParams(type);
   for (unsigned int i=0;i<tmp.size();i++)
   {
      ParameterText *newText = new ParameterText;
      newText->name = tmp[i]->name;
      //FIXME: This is just a temporary kludge
      if (tmp[i]->type == "any")
	 newText->type = "int";
      else
	 newText->type = tmp[i]->type;
      newText->value = tmp[i]->value;
      newText->description = tmp[i]->description;
      textParams.insert(textParams.end(), newText);
   }
}

UINodeParameters::~UINodeParameters()
{
   for (unsigned int i=0;i<textParams.size();i++)
      delete textParams[i];
}

void UINodeParameters::insertLoadedParam(ParameterText *param, string type, string value)
{
  //cerr<<"UINodeParameters::insertLoadedParam"<<endl;
}

void UINodeParameters::load(xmlNodePtr xml_node)
{
   //cerr << "xml_node = " << xml_node << endl;
   xmlNodePtr par = xml_node->children;
   //cerr << "par = " << par << endl;
   
   while (par)
   {
     //cerr<<"par->name "<<par->name<<endl;
      if (string((char*)par->name) == "Parameter")
      {       
	 char *str_name = (char *) xmlGetProp(par, (xmlChar *)"name");
	 char *str_type = (char *) xmlGetProp(par, (xmlChar *)"type");
	 char *str_value = (char *) xmlGetProp(par, (xmlChar *)"value");
	 char *str_description = (char *) xmlGetProp(par, (xmlChar *)"description");

         string name = string (str_name);
         string type = string (str_type);
         string value = string (str_value);
	 string description;

	 if (str_description) {
	   description = str_description;
	 }

	 //cerr<<"name :"<<name<<endl;
	 //cerr<<"type :"<<type<<endl;
	 //cerr<<"value : "<<value<<endl;
	 //cerr<<"description : "<<description<<endl;
	 
	 if (str_name) 
	   free(str_name); 
	 if (str_type) 
	   free(str_type); 
	 if (str_value) 
	   free(str_value);
	 if (str_description)
	   free(str_description);
	          
         ParameterText *param = getParamNamed(name);
	 if (param)
	 {
	    param->type = type;
	    param->value = value;
	    param->description = description;

	    //Not used anymore ? (DL), please delete!
	    //cerr<<"insertLoadedParam"<<endl;
	    //insertLoadedParam(param, type, value);
	    //cerr << "<param: " << name << ", " << type << ":" << value << ">\n";
	 } else {
	   if (node) 
	   {
	     cerr << node->getName() <<" : param " << name << " no longer used\n";
	   }
	   else 
	   {
	     cerr <<" : param " << name << " no longer used\n";
	   }
	 }
      } else if (string((char*)par->name) == "Comments")
      {
	 char *str = (char *)xmlNodeGetContent(par);
	 if (str)
	    setComments(string(str));
	 free(str);
      } else if (!xmlIsBlankNode(par)) {
         
	 cerr << "UINodeParameter::unknown param tag\n";
      }
      par = par->next;
      
   }
}



void UINodeParameters::saveXML(xmlNode *root)
{
   // First add all of the ParameterData Nodes to the xml Node
   if (comments != "")
      xmlNewChild(root, NULL, (xmlChar *)"Comments", (xmlChar*)comments.c_str());
   for (unsigned int i=0;i<textParams.size();i++)
   {
      //if (textParams[i]->value != "")
      {
         xmlNodePtr tree = xmlNewChild(root, NULL, (xmlChar *)"Parameter", NULL);
         xmlSetProp(tree, (xmlChar *)"name", (xmlChar *)textParams[i]->name.c_str());
         xmlSetProp(tree, (xmlChar *)"type", (xmlChar *)textParams[i]->type.c_str());
         xmlSetProp(tree, (xmlChar *)"value", (xmlChar *)textParams[i]->value.c_str());
	 xmlSetProp(tree, (xmlChar *)"description", (xmlChar *)textParams[i]->description.c_str());
      }
   }
}

void UINodeParameters::export2net(ostream &out)
{
   for (unsigned int i=0;i<textParams.size();i++)
   {
      if (textParams[i]->value != "")
      {
     if (textParams[i]->type != "string")
        out << "   <param: " << textParams[i]->name << ", " 
        << textParams[i]->type << ":" << textParams[i]->value << ">" << endl;
     else 
        out << "   <param: " << textParams[i]->name << ", " 
        << textParams[i]->type << ":\"" << textParams[i]->value << "\">" << endl;
      }
   }
}

ParameterText *UINodeParameters::getParamNamed(string n)
{
   for (unsigned int i=0;i<textParams.size();i++)
      if (textParams[i]->name == n)
         return (textParams[i]);
   return NULL;
}

void UINodeParameters::insertNetParams(vector<ItemInfo *> &par)
{
  //cerr<<"UINodeParameters::insertNetParams"<<endl;

   for (unsigned int i=0;i<textParams.size();i++) {

     //cerr<<"textParams[i]->value "<<textParams[i]->value<<endl;
     //cerr<<"textParams[i]->type "<<textParams[i]->type<<endl;

      if (textParams[i]->value != "" && textParams[i]->type == "subnet_param") {

	  bool alreadyPresent = false;
	  for (unsigned int j=0;j<par.size();j++) {
	    if (par[j]->name == textParams[i]->value) {
	      alreadyPresent=true;
	    }
	  }

	  if (!alreadyPresent) {
	    ItemInfo *newInfo = new ItemInfo;

	    //Adding type & description information to subnet_params
	    //(DL) 15/12/2003

	    //Since it is a subnet_param, parameter name will appear as
	    //the name entered in the value field
	    newInfo->name = textParams[i]->value;

	    //type should remain the same
	    newInfo->type = textParams[i]->type;
	    
	    //description should remain the same
	    newInfo->description = textParams[i]->description;
	    
	    par.insert(par.end(), newInfo);
	  }
	  // par.insert(par.end(), textParams[i]->value);
      }
   }
}

//Update node parameters when the node is in fact a subnet and the subnet was modified
//Must be careful not to erase what the user entered (things that are still valid)
void UINodeParameters::updateNetParams(vector<ItemInfo *> &par) {

  //cerr<<"UINodeParameters::updateNetParams called"<<endl;
  
  //add new parameters	
  for (int i = 0; i < par.size(); i++) {
    if (!getParamNamed(par[i]->name)) {
      //cerr<<"adding a parameter : "<<par[i]->name<<endl;
      addParameterText(par[i]->name, par[i]->type, par[i]->value, par[i]->description);
    }
  }

  //remove unused parameters (TODO : better implementation)
  for (int i = 0; i <  textParams.size(); i++) {
    bool found = false;
    string name;
    
    for (int j = 0; j < par.size(); j++) {
      if (textParams[i]->name == par[j]->name) {
	found = true;
	break;
      }
    }

    if (!found) {
      //delete this parameter
      //cerr<<"removing parameter : "<<textParams[i]->name<<endl;
      removeParameterText(textParams[i]->name);
    }
  }
}


ParameterText *UINodeParameters::addParameterText(string name, string type,
						  string value, string descr)
{
    ParameterText *textInfo = new ParameterText;
    textInfo->name = name;
    textInfo->value = value;
    textInfo->type = type;
	textInfo->description = descr;
    textParams.insert(textParams.end(), textInfo);
	
	return textInfo;
}

void UINodeParameters::removeParameterText(string nameToRemove)
{
   //ANSI C++ fix
   vector<ParameterText *>::iterator i=textParams.begin();
      while (i != textParams.end())
      {
         if ((*i)->name == nameToRemove)
	 {
            textParams.erase(i);
	    break;
	 }
	 ++i;
      }
    /*for (int i = 0; i < textParams.size(); i++)
	if (textParams[i]->name == nameToRemove)
           textParams.erase(&textParams[i]);
    */
}

ParameterSet *UINodeParameters::build(const ParameterSet &par)
{
   ParameterSet *parameters = new ParameterSet;

   for (unsigned int i=0;i<textParams.size();i++)
   {
      ParameterText *curr = textParams[i];

      //FIXME: Shouldn't have to use const_cast
      if (curr->value != "")
      {
	 ObjectRef value = ObjectParam::stringParam(curr->type, curr->value, const_cast<ParameterSet &> (par));
	 
	 if (!value.isNil())
	    parameters->add(curr->name,value);
      }
   }
   return parameters;
}

void UINodeParameters::genCode(ostream &out)
{
   out << "   ParameterSet parameters;\n";
   out << "   ObjectRef value;\n";
   for (unsigned int i=0;i<textParams.size();i++)
   {
      ParameterText *curr = textParams[i];
      if (curr->value != "")
      {
	 out << "   value = ObjectParam::stringParam(\"" << curr->type << "\", \"" 
	     << curr->value << "\", const_cast<ParameterSet &> (params));\n";
	 out << "   if (!value->isNil())\n";
	 out << "      parameters.add(\"" << curr->name << "\", value);\n";
      }
   }

}


void UINodeParameters::copyParameterText(UINodeParameters *cpy) {

  //deleting already entered parameter text
  for (unsigned int i = 0; i < textParams.size(); i++) {
    delete textParams[i];
  }
  textParams.resize(0);

  //copying all textParameters
  vector<ParameterText *> &text_params = cpy->get_textParams();
  
  for (unsigned int i = 0; i < text_params.size(); i++) {
    
    //copying parameters
    string my_name = text_params[i]->name;
    string my_type = text_params[i]->type;
    string my_value = text_params[i]->value;
    string my_description = text_params[i]->description;
  
    addParameterText(my_name, my_type, my_value, my_description);
  }
}

}//namespace FD
