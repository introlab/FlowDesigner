#include "UINodeParameters.h"
#include <tree.h>
#include "Node.h"
#include "UINode.h"
#include "UINetwork.h"
#include "UIDocument.h"

#include "ParameterSet.h"

class ParamTypeChange {
public:
   char *newParam;
   string &value;
   
};

const vector<string> &UINodeParameters::allTypes()
{
   static vector<string> types;
   static int init=false;
   if (!init)
   {
      types.insert(types.end(), "int");
      types.insert(types.end(), "float");
      types.insert(types.end(), "string");
      types.insert(types.end(), "bool");
      types.insert(types.end(), "subnet_param");
      init=true;
   }
   return types;
}



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
      }
      par = par->next;
      
   }
}



void UINodeParameters::saveXML(xmlNode *root)
{
   // First add all of the ParameterData Nodes to the xml Node
   for (int i=0;i<textParams.size();i++)
   {
      if (textParams[i]->value != "")
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
      if (curr->value == "")
	 continue;
      ObjectRef value;
      if (curr->type == "int")
      {
	 int val = atoi (curr->value.c_str());
	 value = ObjectRef(new Int(val));
      } 
      else if (curr->type == "bool")
      {
	 if (curr->value == "true" || curr->value == "TRUE")
	    value = ObjectRef(new Bool(true));
	 else 
	    value = ObjectRef(new Bool(false));
      } 
      else if (curr->type == "float")
      {
	 float val = atof (curr->value.c_str());
	 value = ObjectRef(new Float(val)); 
      } 
      else if (curr->type == "string")
      {
	 value = ObjectRef(new String(curr->value));         
      } 
      else if (curr->type == "subnet_param")
      {
	 //BUG WARNING
	 //I guess I shouldn't do that...
	 if (par.exist(curr->value))
	    value = const_cast<ParameterSet &> (par).get(curr->value);
	 else
	 {
	    //cerr << "unknown is " << curr->value << ":" << curr->name << endl;
	    continue;
	 }
	 //cerr << "Subnet_params not supported\n";
      } 
      else {
	 cerr << "UNKNOWN PARAM TYPE: \"" << curr->type << "\"" << endl;
      }
      
      parameters->add(curr->name,value);
   }
   return parameters;
   //perform suvstitution
}
