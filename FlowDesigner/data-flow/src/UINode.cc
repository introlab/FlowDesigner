// Copyright (C) 2001 Jean-Marc Valin

extern "C" {
}
#include "UINode.h"
#include "UINetwork.h"
#include "UITerminal.h"
#include "UINodeParameters.h"
#include <libxml/tree.h>
#include "Node.h"
#include "UIDocument.h"
#include "UILink.h"

#include "ParameterSet.h"
#include "Network.h"
#include <algorithm>

//@implements UIClasses

using namespace std;

namespace FD {

UINode::UINode(UINetwork* _net, string _name, string _type, double _x, double _y, bool doInit)
   : destroyed(false)
   , name(_name)
   , net(_net)
   , type(_type)
   , x(_x)
   , y(_y)
   , xtmp(_x)
   , ytmp(_y)
{
   
   if (doInit)
   {
     parameters = newNodeParameters(this,type);

      vector<ItemInfo *> inputname;
      vector<ItemInfo *> outputname;
      inputname = net->getDocument()->getNetInputs(type); 
      outputname = net->getDocument()->getNetOutputs(type); 
      
      for (unsigned int i=0;i<inputname.size();i++)
      {
         inputs.insert(inputs.end(), new UITerminal (inputname[i], 
                                                     this, true, 0.0, 0.0));
      }
      
      for (unsigned int i=0;i<outputname.size();i++) 
      { 
         outputs.insert(outputs.end(), new UITerminal (outputname[i], 
                                                       this, false, 0.0, 0.0));
      }
      
      description = net->getDocument()->getDescription(type);
      
      
   }
      
}

UINode::UINode(UINetwork* _net, xmlNodePtr def, bool doInit)
   : destroyed(false)
   , net(_net)
{
   char *str_name = (char *)xmlGetProp(def, (xmlChar *)"name");
   char *str_type = (char *)xmlGetProp(def, (xmlChar *)"type");
   char *str_x = (char *)xmlGetProp(def, (xmlChar *)"x");
   char *str_y = (char *)xmlGetProp(def, (xmlChar *)"y");

   if (!str_name || !str_type || !str_x || !str_y)
   {
      throw new GeneralException("Missing node parameter(s) in XML definition", __FILE__, __LINE__);
   }

   name = string(str_name);
   type = string(str_type);
   x = atof(str_x);
   y = atof(str_y);

   free (str_name); free (str_type); free(str_x); free(str_y);

   xtmp = x;
   ytmp = y;
  
   if (doInit)
   {
       parameters = newNodeParameters(this, type);
       parameters->load(def);
      
      vector<ItemInfo *> inputname;
      vector<ItemInfo *> outputname;
      {
	 inputname = net->getDocument()->getNetInputs(type); 
	 outputname = net->getDocument()->getNetOutputs(type); 
      }
      
      for (unsigned int i=0;i<inputname.size();i++)
      {
         inputs.insert(inputs.end(), new UITerminal (inputname[i],
                                                     this, true, 0.0, 0.0));
      }
      
      for (unsigned int i=0;i<outputname.size();i++)
      {
         outputs.insert(outputs.end(), new UITerminal (outputname[i], 
                                                       this, false, 0.0, 0.0));
      }

      description = net->getDocument()->getDescription(type);
      
      
      
   }
}

UINode::~UINode()
{
   if (!destroyed)
   {
      for (unsigned int i=0;i<inputs.size();i++)
	 delete inputs[i];
      for (unsigned int i=0;i<outputs.size();i++)
	 delete outputs[i];
      delete parameters;
      net->removeNode(this);
   }
}

void UINode::saveXML(xmlNode *root)
{
   xmlNodePtr tree = xmlNewChild(root, NULL, (xmlChar *)"Node", NULL);
   xmlSetProp(tree, (xmlChar *)"name", (xmlChar *)name.c_str());
   xmlSetProp(tree, (xmlChar *)"type", (xmlChar *)type.c_str());
   char tmp[15];
   sprintf (tmp, "%f", float(x));
   xmlSetProp(tree, (xmlChar *)"x", (xmlChar *)tmp);
   sprintf (tmp, "%f", float(y));
   xmlSetProp(tree, (xmlChar *)"y", (xmlChar *)tmp);
   parameters->saveXML(tree);
}


UITerminal *UINode::getInputNamed(string n)
{
   for (unsigned int i=0;i<inputs.size();i++)
      if (inputs[i]->getName() == n)
         return inputs[i];
   return NULL;
}

UITerminal *UINode::getOutputNamed(string n)
{
   for (unsigned int i=0;i<outputs.size();i++)
      if (outputs[i]->getName() == n)
         return outputs[i];
   return NULL;
}

void UINode::setNodeParameters(UINodeParameters *params)
{
    parameters = params;
}

void UINode::insertNetParams(vector<ItemInfo *> &params)
{
   parameters->insertNetParams(params);
}

void UINode::updateNetParams(vector<ItemInfo *> &params) {
  parameters->updateNetParams(params);
}

UILink *UINode::newLink (UITerminal *_from, UITerminal *_to)
{

  //cerr<<"UINode::newLink\n";
  return new UILink (_from, _to);
}

UINetTerminal *UINode::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const string &_name,
				       const string &_objType, const string &_description) {
  //cerr<<"UINode::newNetTerminal\n";
  return new UINetTerminal (_terminal, _type, _name, _objType, _description);
}

UINodeParameters *UINode::newNodeParameters (UINode *_node, string type)
{
  //cerr << "UINode::newNodeParameters\n";
  return new UINodeParameters (_node, type);
}

Node *UINode::build(const ParameterSet &params)
{
   //for all params, it will perform substitution in parameters (process subnet_params)

   Node *node=NULL;
   _NodeFactory *factory = NULL;
   ParameterSet *par=NULL;
   factory = Node::getFactoryNamed(type);
   try {
      par = parameters->build(params);
      //This is only true if type is in the dictionary
      if (factory) 
      {
	 node = factory->Create(name, *par);
      } else {
	 UINetwork *buildNet = net->getDocument()->getNetworkNamed(type);
	 if (buildNet)
	    node = buildNet->build(name, *par);
	 else
	 { 
	    node = UIDocument::buildExternal(type, name, *par);
	    if (!node)
	    {
	       throw new GeneralException(string("Node not found: ")+type, __FILE__, __LINE__);
	    }
	 }
      }
   } catch (BaseException *e)
   {
      if (par)
         delete par;
      if (node)
	 delete node;
      throw e->add (new GeneralException(string("Exception caught while creating ")+name 
					 + " (type " + type + ")", __FILE__, __LINE__));
   }
   
   node->setUINode(this);

   delete par;
   return node;
   
}

void UINode::addTerminal(const string &_name, UINetTerminal::NetTermType _type, const string &_objType, const string &_description) 
{

   double x1=0,y1=0,x2=0,y2=0;
   ItemInfo info;
   
   info.name = _name;
   info.type = _objType;
   info.description = _description;
   
   switch (_type) {

   case UINetTerminal::INPUT:

      inputs.insert(inputs.end(), new UITerminal (&info, this, true, x1, y1));
    
      break;

   case UINetTerminal::OUTPUT:

      outputs.insert(outputs.end(), new UITerminal (&info, this, false, x2,y2 ));
    
      break;

   default:
      break;

   }
  
   redraw();
}


void UINode::removeTerminal(const string &_name, UINetTerminal::NetTermType _type)
{
   vector<UITerminal*>::iterator term;
   switch (_type) {
      
   case UINetTerminal::INPUT:
      
      term = find(inputs.begin(), inputs.end(), getInputNamed(_name));
      if (term!=inputs.end())
      {
         delete *term;
         inputs.erase(term);
      }

      break;
      
   case UINetTerminal::OUTPUT:

      term = find(outputs.begin(), outputs.end(), getOutputNamed(_name));
      if (term!=outputs.end())
      {
         delete *term;
         outputs.erase(term);
      }

      break;
      
   default:
      break;
      
   }
   redraw();
}


void UINode::genCode(ostream &out, int &id, set<string> &nodeList)
{
   int bakID=id;
   id++;

   int bakID2=id;

   bool builtin=false;
   _NodeFactory *factory = NULL;
   factory = Node::getFactoryNamed(type);
   if (factory)
   {
      builtin=true;
      nodeList.insert(nodeList.end(), type);
   }
   else
   {
      builtin=false;
      UINetwork *buildNet = net->getDocument()->getNetworkNamed(type);
      if (buildNet)
	 buildNet->genCode(out, id, nodeList);
      else {
	 UIDocument::genCodeExternal(type, out, id, nodeList);
      }
   }
   out << "static Node *genNode" << bakID << "(const ParameterSet &params)\n";
   out << "{\n";

   parameters->genCode(out);


   
   if (builtin) 
   {
      out << "   _NodeFactory *factory = Node::getFactoryNamed(\"" << type << "\");\n";
      out << "   if (!factory)\n";
      out << "      throw new GeneralException(\"Node could not be found: " << type << "\", __FILE__, __LINE__);\n";
      out << "   Node *node = factory->Create(\""<<name << "\", parameters);\n";
   } else {
      out << "   Node *node = genNet" << bakID2 << "(\""<<name << "\", parameters);\n";
   }

   out << "   return node;\n";

   out << "}\n\n";
}


string UINode::getComments() const 
{
   return parameters->getComments();
}

void UINode::rename (const string &newName) {

  //FIXME : should do something about the node name?

  type = newName;

}

}//namespace FD
