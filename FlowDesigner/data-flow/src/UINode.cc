//#include <gnome.h>
extern "C" {
//#include "controls.h"
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

/*template <class T> const T & max(const T &x, const T &y) {return x > y ? x : y;}
template <class T> T & max(T &x, T &y) {return x > y ? x : y;}
template <class T> const T & min(const T &x, const T &y) {return x < y ? x : y;}
template <class T> T & min(T &x, T &y) {return x < y ? x : y;}
*/



UINode::UINode(UINetwork* _net, string _name, string _type, double _x, double _y, bool doInit)
   : net(_net)
   , name(_name)
   , type(_type)
   , x(_x)
   , xtmp(_x)
   , y(_y)
   , ytmp(_y)
   , destroyed(false)
   //, dragging(false)
   //, grab(false)
{
   //draw();

   if (doInit)
     {
      
       

       vector<ItemInfo *> inputname;
       vector<ItemInfo *> outputname;
       //_NodeFactory *factory = Node::getFactoryNamed(type);
       //if (factory)
       //{
       //inputname = factory->getInputs();
       //outputname = factory->getOutputs();
       //} else {
       inputname = net->getDocument()->getNetInputs(type); 
       outputname = net->getDocument()->getNetOutputs(type); 
       //cerr << "UINode::draw factory not found in simple nodes\n";
       
       for (int i=0;i<inputname.size();i++) {
         inputs.insert(inputs.end(), new UITerminal (inputname[i], 
                                                     this, true, 0.0, 0.0));
       }

       for (int i=0;i<outputname.size();i++) {
         outputs.insert(outputs.end(), new UITerminal (outputname[i], 
                                                       this, false, 0.0, 0.0));
       }
		 
       description = net->getDocument()->getDescription(type);

       parameters = newNodeParameters(this,type);

     }
      
   //parameters->show();
   //createPopup();
}

UINode::UINode(UINetwork* _net, xmlNodePtr def, bool doInit)
   : net(_net)
   , destroyed(false)
   //, dragging(false)
   //, grab(false)
{
   name = string((char *)xmlGetProp(def, (CHAR *)"name"));
   type = string((char *)xmlGetProp(def, (CHAR *)"type"));
   x = atof((char *)xmlGetProp(def, (CHAR *)"x"));
   y = atof((char *)xmlGetProp(def, (CHAR *)"y"));
   xtmp = x;
   ytmp=y;
   //draw();

   if (doInit)
   {
      

      vector<ItemInfo *> inputname;
      vector<ItemInfo *> outputname;
      /*_NodeFactory *factory = Node::getFactoryNamed(type);
      if (factory)
      {
     inputname = factory->getInputs();
     outputname = factory->getOutputs();
     } else*/ {
     inputname = net->getDocument()->getNetInputs(type); 
     outputname = net->getDocument()->getNetOutputs(type); 
     //cerr << "UINode::draw factory not found in simple nodes\n";
      }

      for (int i=0;i<inputname.size();i++) 
         inputs.insert(inputs.end(), new UITerminal (inputname[i],
                                                     this, true, 0.0, 0.0));

      
      for (int i=0;i<outputname.size();i++)
         outputs.insert(outputs.end(), new UITerminal (outputname[i], 
                                                       this, false, 0.0, 0.0));
		 
	  description = net->getDocument()->getDescription(type);


	  parameters = newNodeParameters(this, type);
	  //cerr << "ici\n";
	  parameters->load(def);

   }
      //parameters->show();
      //createPopup();
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

/*void UINode::setAsCondition()
{
   net->setCondition(this);
   //gnome_canvas_item_set(nodeRect, "fill_color_rgba", 0xff000040, NULL);
}

void UINode::unsetAsCondition()
{
   //gnome_canvas_item_set(nodeRect, "fill_color_rgba", 0x3cb37120, NULL);
}
*/

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

/*UITerminal *UINode::newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y)
{
   return new UITerminal (_name, _node, _isInput, _x, _y);
}*/

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
   if (factory) 
   {
      node = factory->Create(name, *par);
   } else {
      UINetwork *buildNet = net->getDocument()->getNetworkNamed(type);
      if (buildNet)
	 node = buildNet->build(name, *par);
      else
      { 
	 //cerr << "building external\n";
	 node = UIDocument::buildExternal(type, name, *par);
	 //cerr << "done\n";
      }
   }
   node->setUINode(this);

   //cerr << "done\n";
   /*if (name == "node1")
   {
      cout << "params for node: " << name << " (" << net->getName() << ")\n";
      par->print();
      cout << endl;
      }*/
//cerr << "node created\n";
   return node;
   
}