// Copyright (C) 2001 Jean-Marc Valin

#include "UINodeParameters.h"
#include <tree.h>
#include "Node.h"
#include "UINode.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include "object_param.h"
#include "ParameterSet.h"

class ParamTypeChange {
public:
   char *newParam;
   string &value;
   
};

UINodeParameters::UINodeParameters(UINode *_node, string type)
   : node(_node)
{
   //cerr << "UINodeParameters::UINodeParameters\n";
   int i;
   /*_NodeFactory *factory = Node::getFactoryNamed(type);
   if (factory)
   {
      vector<string> tmp=factory->getParams();
      textParams.resize(tmp.size());
      for (i=0;i<tmp.size();i++)
     textParams[i].name=tmp[i];
     } else*/ {
      vector<ItemInfo *> tmp = node->getNetwork()->getDocument()->getNetParams(type);
//      textParams.resize(tmp.size());
      for (i=0;i<tmp.size();i++)
          {
              ParameterText *newText = new ParameterText;
              newText->name = tmp[i]->name;
              newText->type = tmp[i]->type;
              newText->value = tmp[i]->value;
              newText->description = tmp[i]->description;
              textParams.insert(textParams.end(), newText);
          }
      //cerr << "Factory not found in simple nodes... must be a subnet...\n";
   }

}

UINodeParameters::~UINodeParameters()
{
   for (int i=0;i<textParams.size();i++)
      delete textParams[i];
}

void UINodeParameters::insertLoadedParam(ParameterText *param, string type, string value)
{
}

void UINodeParameters::load(xmlNodePtr node)
{
   //cerr << "node = " << node << endl;
   xmlNodePtr par = node->childs;
   //cerr << "par = " << par << endl;
   while (par)
   {
      if (string((char*)par->name) == "Parameter")
      {
         string name = string ((char *) xmlGetProp(par, (CHAR *)"name"));
         string type = string ((char *) xmlGetProp(par, (CHAR *)"type"));
         string value = string ((char *) xmlGetProp(par, (CHAR *)"value"));
         
         ParameterText *param = getParamNamed(name);
	 if (param)
	 {
	    param->type = type;
	    param->value = value;
	    insertLoadedParam(param, type, value);
	    //cerr << "<param: " << name << ", " << type << ":" << value << ">\n";
	 } else {
	    //cerr << "param " << name << " no longer used\n";
	 }
      } else if (string((char*)par->name) == "Comments")
      {
	 char *str = (char *)xmlNodeGetContent(par);
	 if (str)
	    setComments(string(str));
      } else {
	 cerr << "unknown param tag\n";
      }
      par = par->next;
      
   }
}



void UINodeParameters::saveXML(xmlNode *root)
{
   // First add all of the ParameterData Nodes to the xml Node
   if (comments != "")
      xmlNewChild(root, NULL, (CHAR *)"Comments", (xmlChar*)comments.c_str());
   for (int i=0;i<textParams.size();i++)
   {
      //if (textParams[i]->value != "")
      {
         xmlNodePtr tree = xmlNewChild(root, NULL, (CHAR *)"Parameter", NULL);
         xmlSetProp(tree, (CHAR *)"name", (CHAR *)textParams[i]->name.c_str());
         xmlSetProp(tree, (CHAR *)"type", (CHAR *)textParams[i]->type.c_str());
         xmlSetProp(tree, (CHAR *)"value", (CHAR *)textParams[i]->value.c_str());
      }
   }
}

void UINodeParameters::export2net(ostream &out)
{
   for (int i=0;i<textParams.size();i++)
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
   for (int i=0;i<textParams.size();i++)
      if (textParams[i]->name == n)
         return (textParams[i]);
   return NULL;
}

void UINodeParameters::insertNetParams(vector<ItemInfo *> &par)
{
   for (int i=0;i<textParams.size();i++)
   {
      if (textParams[i]->value != "" && textParams[i]->type == "subnet_param")
      {
     bool alreadyPresent = false;
     for (int j=0;j<par.size();j++)
        if (par[j]->name == textParams[i]->value)
           alreadyPresent=true;
     if (!alreadyPresent) 
         {
            ItemInfo *newInfo = new ItemInfo;
            newInfo->name = textParams[i]->value;
            par.insert(par.end(), newInfo);
         }
        // par.insert(par.end(), textParams[i]->value);
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

   for (int i=0;i<textParams.size();i++)
   {
      ParameterText *curr = textParams[i];

      //FIXME: Shouldn't have to use const_cast
      if (curr->value != "")
      {
	 ObjectRef value = ObjectParam::stringParam(curr->type, curr->value, const_cast<ParameterSet &> (par));
	 
	 if (!value->isNil())
	    parameters->add(curr->name,value);
      }
   }
   return parameters;
}

void UINodeParameters::genCode(ostream &out)
{
   out << "   ParameterSet parameters;\n";
   out << "   ObjectRef value;\n";
   for (int i=0;i<textParams.size();i++)
   {
      ParameterText *curr = textParams[i];
      if (curr->value != "")
      {
	 out << "   value = ObjectParam::stringParam(\"" << curr->type << "\", \"" 
	     << curr->value << "\", const_cast<ParameterSet &> (params));\n";
	 out << "   if (value->status == Object::valid)\n";
	 out << "      parameters.add(\"" << curr->name << "\", value);\n";
      }
   }

}
