// Copyright (C) 2001 Jean-Marc Valin

extern "C" {
}
#include "UINode.h"
#include "UINetwork.h"
#include "UITerminal.h"
#include "UINodeParameters.h"
#include <tree.h>
#include "Node.h"
#include "UIDocument.h"
#include "UILink.h"

#include "ParameterSet.h"
#include "Network.h"

//@implements UIClasses


UINode::UINode(UINetwork* _net, string _name, string _type, double _x, double _y, bool doInit)
   : net(_net)
   , name(_name)
   , type(_type)
   , x(_x)
   , xtmp(_x)
   , y(_y)
   , ytmp(_y)
   , destroyed(false)
{

   parameters = newNodeParameters(this,type);
   
   if (doInit)
   {
      vector<ItemInfo *> inputname;
      vector<ItemInfo *> outputname;
      inputname = net->getDocument()->getNetInputs(type); 
      outputname = net->getDocument()->getNetOutputs(type); 
      
      for (int i=0;i<inputname.size();i++)
      {
         inputs.insert(inputs.end(), new UITerminal (inputname[i], 
                                                     this, true, 0.0, 0.0));
         //FIXME: Fixing this leak cleanly requires a good cleanup
         //delete outputname[i];
      }
      
      for (int i=0;i<outputname.size();i++) 
      { 
         outputs.insert(outputs.end(), new UITerminal (outputname[i], 
                                                       this, false, 0.0, 0.0));
	 
         //FIXME: Fixing this leak cleanly requires a good cleanup
         //delete outputname[i];
      }
      
      description = net->getDocument()->getDescription(type);
      
      
   }
      
}

UINode::UINode(UINetwork* _net, xmlNodePtr def, bool doInit)
   : net(_net)
   , destroyed(false)
{
   name = string((char *)xmlGetProp(def, (CHAR *)"name"));
   type = string((char *)xmlGetProp(def, (CHAR *)"type"));
   x = atof((char *)xmlGetProp(def, (CHAR *)"x"));
   y = atof((char *)xmlGetProp(def, (CHAR *)"y"));
   xtmp = x;
   ytmp=y;
   //draw();

   parameters = newNodeParameters(this, type);
   //cerr << "ici\n";
   parameters->load(def);

   if (doInit)
   {
      
      
      vector<ItemInfo *> inputname;
      vector<ItemInfo *> outputname;
      {
	 inputname = net->getDocument()->getNetInputs(type); 
	 outputname = net->getDocument()->getNetOutputs(type); 
	 //cerr << "UINode::draw factory not found in simple nodes\n";
      }
      
      for (int i=0;i<inputname.size();i++)
      {
         inputs.insert(inputs.end(), new UITerminal (inputname[i],
                                                     this, true, 0.0, 0.0));
         //FIXME: Fixing this leak cleanly requires a good cleanup
         //delete outputname[i];
      }
      
      for (int i=0;i<outputname.size();i++)
      {
         outputs.insert(outputs.end(), new UITerminal (outputname[i], 
                                                       this, false, 0.0, 0.0));
         //FIXME: Fixing this leak cleanly requires a good cleanup
         //delete outputname[i];
      }

      description = net->getDocument()->getDescription(type);
      
      
      
   }
}

UINode::~UINode()
{
   if (!destroyed)
   {
      for (int i=0;i<inputs.size();i++)
     delete inputs[i];
      for (int i=0;i<outputs.size();i++)
     delete outputs[i];
      delete parameters;
      net->removeNode(this);
   }
}

void UINode::saveXML(xmlNode *root)
{
   xmlNodePtr tree = xmlNewChild(root, NULL, (CHAR *)"Node", NULL);
   xmlSetProp(tree, (CHAR *)"name", (CHAR *)name.c_str());
   xmlSetProp(tree, (CHAR *)"type", (CHAR *)type.c_str());
   char tmp[15];
   sprintf (tmp, "%f", float(x));
   xmlSetProp(tree, (CHAR *)"x", (CHAR *)tmp);
   sprintf (tmp, "%f", float(y));
   xmlSetProp(tree, (CHAR *)"y", (CHAR *)tmp);
   parameters->saveXML(tree);
}

void UINode::export2net (ostream &out)
{
   out << "   <node: " << name << "> <type: " << type << ">" << endl;
   parameters->export2net(out);
   for (int i=0;i<inputs.size();i++)
      inputs[i]->export2net(out);
   out << endl;
}

UITerminal *UINode::getInputNamed(string n)
{
   for (int i=0;i<inputs.size();i++)
      if (inputs[i]->getName() == n)
         return inputs[i];
   return NULL;
}

UITerminal *UINode::getOutputNamed(string n)
{
   for (int i=0;i<outputs.size();i++)
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


UILink *UINode::newLink (UITerminal *_from, UITerminal *_to)
{
   return new UILink (_from, _to);
}

UINetTerminal *UINode::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name)
{
   return new UINetTerminal (_terminal, _type, _name);
}

UINodeParameters *UINode::newNodeParameters (UINode *_node, string type)
{
   //cerr << "UINode::newNodeParameters\n";
   return new UINodeParameters (_node, type);
}

Node *UINode::build(const ParameterSet &params)
{
   //cerr << "UINode::build for node" << name << endl;
   //params.print();
   //for all params, it will perform substitution in parameters (process subnet_params)
   //cerr << "building parameters\n";
   ParameterSet *par = parameters->build(params);

   //This is only true is type is in the dictionary
   Node *node=NULL;
   _NodeFactory *factory = NULL;
   factory = Node::getFactoryNamed(type);
   //cerr << "building " << type << endl;
   try {
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
      if (node)
	 delete node;
      throw e->add (new GeneralException(string("Exception caught while creating ")+name 
					 + " (type " + type + ")", __FILE__, __LINE__));
   }
   
   node->setUINode(this);

   delete par;
   return node;
   
}

void UINode::genCode(ostream &out, int &id, set<string> &nodeList)
{
   int bakID=id;
   id++;

   int bakID2=id;

   bool builtin=false;
   Node *node=NULL;
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
	 //throw new GeneralException("external nodes not supported yet\n", __FILE__, __LINE__);
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
      //FIXME: Should still for (factory == NULL) in generated code and report errors
      out << "   Node *node = factory->Create(\""<<name << "\", parameters);\n";
   } else {
      out << "   Node *node = genNet" << bakID2 << "(\""<<name << "\", parameters);\n";
   }

   out << "   return node;\n";

   out << "}\n\n";
}
