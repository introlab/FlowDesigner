#ifndef UINODE_H
#define UINODE_H

//#include <gnome.h>
#include <string>
#include <vector>
#include <string>
#include <tree.h>
#include <fstream>

#include "UINetTerminal.h"

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
   bool destroyed;
   string name;

   UINetwork *net;


   string type;
   
   string description;

   double x,y;

   double xtmp,ytmp;

   vector <UITerminal *> inputs;

   vector <UITerminal *> outputs;

   UINodeParameters *parameters;

public:

   UINode(UINetwork* _net, string _name, string _type, double x, double y, bool doInit=1);

   UINode(UINetwork* _net, xmlNodePtr def, bool doInit=1);

   virtual ~UINode();


   const string &getName() {return name;}
   const string &getType() {return type;}

   UINetwork * getNetwork() {return net;}

   void saveXML(xmlNode *root);

   UITerminal *getInputNamed(string n);

   UITerminal *getOutputNamed(string n);

   void getPos (double &xx, double &yy)
   {
      xx=x;
      yy=y;
   }
   
   void setPos (double new_x, double new_y)
   {
	   x = new_x;
	   y = new_y;
   }

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
   
   vector<UITerminal *> getInputs() {return inputs;}
   vector <UITerminal *> getOutputs() {return outputs;}
   UINodeParameters * getParameters() {return parameters;}
   string getDescription() {return description;}

   friend class UINetwork;
//   friend GUINetwork;
};


#endif
