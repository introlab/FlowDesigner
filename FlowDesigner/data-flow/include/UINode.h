// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINODE_H
#define UINODE_H

//#include <gnome.h>
#include <string>
#include <vector>
#include <string>
#include <tree.h>
#include <fstream>

#include "UINetTerminal.h"
#include <set>

using namespace std;

class UINetwork;
//class GUINetwork;
class UINodeParameters;
class UILink;
class ItemInfo;

class UINetTerminal;
class UITerminal;

class Node;
class ParameterSet;

/** UINode is the represantation used to store data for any node, 
    either in the GUI or in batch mode. A UINode cannot perform any
    operation but can be used to build a real Node.
    @author Jean-Marc Valin
*/
class UINode {
protected:
   /**Not too sure what I was thinking when I wrote that*/
   bool destroyed;

   /**Node name*/
   string name;

   /**The network in which the node is included*/
   UINetwork *net;

   /**Node type (either the name of the .n or the builtin Node subclass)*/
   string type;
   
   /**Node description (unused?)*/
   string description;

   /**Position on the canvas*/
   double x,y;

   /**Temporary position used in move operations*/
   double xtmp,ytmp;

   /**Pointers to all the inputs*/
   vector <UITerminal *> inputs;

   /**Pointers to all the outputs*/
   vector <UITerminal *> outputs;

   /**All the node parameters*/
   UINodeParameters *parameters;

public:

   /**"Normal" constructor*/
   UINode(UINetwork* _net, string _name, string _type, double x, double y, bool doInit=1);

   /**Constructor from XML parse tree*/
   UINode(UINetwork* _net, xmlNodePtr def, bool doInit=1);

   /**Destructor*/
   virtual ~UINode();

   /**Returns the node name*/
   const string &getName() {return name;}
   
   /**Returns the node type*/
   const string &getType() {return type;}

   /**Returns the corresponding network*/
   UINetwork * getNetwork() {return net;}

   /**Save to an XML parse tree*/
   void saveXML(xmlNode *root);

   /**Returns the input (terminal) corresponding to a certain name*/
   UITerminal *getInputNamed(string n);

   /**Returns the output (terminal) corresponding to a certain name*/
   UITerminal *getOutputNamed(string n);

   /**Returns the node position*/
   void getPos (double &xx, double &yy)
   {
      xx=x;
      yy=y;
   }
   
   /**Changes the position (not too sure it should be used*/
   void setPos (double new_x, double new_y)
   {
	   x = new_x;
	   y = new_y;
   }

   /**Export to old network format (deprecated)*/
   void export2net (ostream &out);
/*
   virtual void setAsCondition();
   
   virtual void unsetAsCondition();
*/   
   
   void setNodeParameters(UINodeParameters *params);   
   
   void insertNetParams(vector<ItemInfo *> &params);

//   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);

   virtual void notifyError(const string &message) {}

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to);

   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name);

   virtual UINodeParameters *newNodeParameters (UINode *_node, string type);

   Node *build(const ParameterSet &params);

   /**Generate C++ code for building the document, instead of using XML*/
   void genCode(ostream &out, int &id, set<string> &nodeList);

   vector<UITerminal *> getInputs() {return inputs;}
   vector <UITerminal *> getOutputs() {return outputs;}
   UINodeParameters * getParameters() {return parameters;}
   string getDescription() {return description;}

   friend class UINetwork;
//   friend GUINetwork;
};


#endif
