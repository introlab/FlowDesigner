#ifndef UINETWORK_H
#define UINETWORK_H

//#include <gnome.h>
//#include "UIDocument.h"
#include <vector>
#include <string>
#include <tree.h>
#include <fstream>
#include <pthread.h>
#include "UINetTerminal.h"

//struct xmlNode;
class ItemInfo;
class UINode;
class UITerminal;
class UILink;
class UINodeMenu;
class UIDocument;
//class UINetTerminal;

class Network;
class ParameterSet;

class UINetwork {
  public:
   enum Type {subnet=0, iterator=1, threaded=2};
protected:
   bool destroyed;
   UIDocument *doc;
   string name;
   Type type;
   vector <UINode *> nodes;
   vector <UILink *> links;

   vector <UINetTerminal *> terminals;

   ///The condition node of the iterator (no meaning for subnets)
   //UINode *conditionNode;
public:
   UINetwork(UIDocument *_doc, string _name, Type _type);

   UINetwork(UIDocument *_doc, xmlNodePtr net, bool init=true);

   void load (xmlNodePtr net);

   virtual ~UINetwork();

   void setModified();

   UINode *loadNode (xmlNodePtr node);
   
   UINode *getNodeNamed(string n);
   
   void addNode(UINode *node);

   void removeNode (UINode *node);

   void addLink (UILink *link);

   void removeLink (UILink *link);

   const string &getName() {return name;}
   
   Type getType() {return type;}

   UIDocument *getDocument() {return doc;}

   bool isIter() {return type==iterator;}


   void saveXML(xmlNode *root);

   void export2net (ostream &out);

   //void setCondition(UINode *cond);

   virtual void newNetNotify(const string &cat, const string &type);

   void addTerminal(UINetTerminal *term) 
      {terminals.insert(terminals.end(), term);}

   void removeTerminal(UINetTerminal *term) 
   {
      //Now, this should comply to ANSI C++
      vector<UINetTerminal *>::iterator i=terminals.begin();
      while (i != terminals.end())
      {
         if (*i == term)
	 {
            terminals.erase(i);
	    break;
	 }
	 ++i;
      }
	 /*for (int i=0;i<terminals.size();i++)
         if (terminals[i]==term)
            terminals.erase(&terminals[i]);
      */
   }

   vector<string> getTerminals(UINetTerminal::NetTermType termType);

   void insertNetParams(vector<ItemInfo *> &params);

   virtual UINode *newNode(UINetwork* _net, xmlNodePtr def);
   
   virtual UINode *newNode(UINetwork* _net, string _name, 
						   string _type, double _x, double _y, bool doInit);

//   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to, char *str);

   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name);

   Network *build(const string &netName, const ParameterSet &params);
   
	vector<UINode *> getNodes() {return nodes;}
	vector<UILink *> getLinks() {return links;}
	vector<UINetTerminal *> getTerminals() {return terminals;}

};


#endif
