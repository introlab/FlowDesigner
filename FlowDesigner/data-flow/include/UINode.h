// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINODE_H
#define UINODE_H

//#include <gnome.h>
#include <string>
#include <vector>
#include <string>
#include <libxml/tree.h>
#include <fstream>
#include "UINetTerminal.h"
#include <set>

namespace FD {

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
   std::string name;

   /**The network in which the node is included*/
   UINetwork *net;

   /**Node type (either the name of the .n or the builtin Node subclass)*/
   std::string type;
   
   /**Node description (unused?)*/
   std::string description;

   /**Position on the canvas*/
   double x,y;

   /**Temporary position used in move operations*/
   double xtmp,ytmp;

   /**Pointers to all the inputs*/
   std::vector <UITerminal *> inputs;

   /**Pointers to all the outputs*/
   std::vector <UITerminal *> outputs;

   /**All the node parameters*/
   UINodeParameters *parameters;


public:

   /**"Normal" constructor*/
   UINode(UINetwork* _net, std::string _name, std::string _type, double x, double y, bool doInit=1);

   /**Constructor from XML parse tree*/
   UINode(UINetwork* _net, xmlNodePtr def, bool doInit=1);

   /**Destructor*/
   virtual ~UINode();

   /**Returns the node name*/
   const std::string &getName() {return name;}
   
   /**Returns the node type*/
   const std::string &getType() {return type;}

   /**Returns the corresponding network*/
   UINetwork * getNetwork() {return net;}


   /**Rename a node (when network included as a subnet)*/
   virtual void rename (const std::string &newName);

   /**Adds a new terminal to a node*/
   virtual void addTerminal(const std::string &_name, UINetTerminal::NetTermType _type, 
			    const std::string &_objType="any", const std::string &_description="No description available");

   /**Removes a terminal from a node*/
   virtual void removeTerminal(const std::string &_name, UINetTerminal::NetTermType _type);

   /**Save to an XML parse tree*/
   void saveXML(xmlNode *root);

   /** Load from an XML tree */
   void loadXML(xmlNodePtr def);

   /**Returns the input (terminal) corresponding to a certain name*/
   UITerminal *getInputNamed(std::string n);

   /**Returns the output (terminal) corresponding to a certain name*/
   UITerminal *getOutputNamed(std::string n);

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

  

   void setNodeParameters(UINodeParameters *params);   
   
   void insertNetParams(std::vector<ItemInfo *> &params);

   void updateNetParams(std::vector<ItemInfo *> &params);

//   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);

   virtual void notifyError(const std::string &message) {}

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to);
   
   virtual UITerminal* newTerminal(ItemInfo *_info, UINode *_node, bool _isInput, double _x, double _y);

   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name,
					  const std::string &_objType="any", const std::string &_description="No description available");

   virtual UINodeParameters *newNodeParameters (UINode *_node, std::string type);
 
   virtual void redraw() {}

   Node *build(const ParameterSet &params);

   /**Generate C++ code for building the document, instead of using XML*/
   void genCode(std::ostream &out, int &id, std::set<std::string> &nodeList);

   std::vector<UITerminal *> getInputs() {return inputs;}
   std::vector <UITerminal *> getOutputs() {return outputs;}
   UINodeParameters * getParameters() {return parameters;}
   std::string getDescription() {return description;}
   std::string getComments() const;

   friend class UINetwork;
//   friend GUINetwork;
};

}//namespace FD
#endif
