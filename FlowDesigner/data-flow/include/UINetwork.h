// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINETWORK_H
#define UINETWORK_H

//#include <gnome.h>
//#include "UIDocument.h"
#include <vector>
#include <string>
#include <libxml/tree.h>
#include <fstream>
#include <pthread.h>
#include "UINetTerminal.h"
#include "UINodeParameters.h"
#include "UINodeRepository.h"
#include <set>

//struct xmlNode;
class ItemInfo;
class UINode;
class UITerminal;
class UILink;
class UINodeMenu;
class UIDocument;
class UINote;
//class UINetTerminal;

class Network;
class ParameterSet;

/** UINetwork is the represantation used to store data for any network, 
    either in the GUI or in batch mode. A UINetwork cannot perform any
    operation but can be used to build a real Network.
    @author Jean-Marc Valin
*/
class UINetwork {
  public:
   /**Available network types*/
   enum Type {subnet=0, iterator=1, threaded=2};
protected:

   /**Has the network been destroyed? Not too sure what I was thinking...*/
   bool destroyed;

   /**Document (.n file) to which the network belongs*/
   UIDocument *doc;

   /**Name of the network*/
   string name;

   /**Description of the network*/
   string m_description;

   /**Network type (subnet, iterator, threaded iterator*/
   Type type;

   /**All the links included in the network*/
   vector <UINode *> nodes;

   /**All the links included in the network*/
   vector <UILink *> links;

   /**Pointer to all the network terminals*/
   vector <UINetTerminal *> terminals;

   /**All the notes in the network*/
   vector <UINote*> m_notes;

   ///The condition node of the iterator (no meaning for subnets)
   //UINode *conditionNode;
private:
   
   /**Used to determine infinite recursion in build*/
   bool buildRecurs;
public:
   /**Basic constructor, allows building the UINetwork gradually*/
   UINetwork(UIDocument *_doc, string _name, Type _type);

   /**Construct a UINetwork from a parsed XML file*/
   UINetwork(UIDocument *_doc, xmlNodePtr net, bool init=true);

   /**Loads the info from the XML parse tree*/
   void load (xmlNodePtr net);

   /**Destructor*/
   virtual ~UINetwork();

   /**Informs that some part of the network has been changed. To be called anytime one modifies something in the network, a node, a link, ...*/
   void setModified();

   UINode *loadNode (xmlNodePtr node);
   
   UINode *getNodeNamed(string n);
   
   void addNode(UINode *node);

   void removeNode (UINode *node);

   void addLink (UILink *link);

   void removeLink (UILink *link);

   const string &getName() {return name;}

   string getDescription() {return m_description;}

   void setDescription(const string & _description){m_description = _description;}

   
   Type getType() {return type;}

   UIDocument *getDocument() {return doc;}

   bool isIter() {return type==iterator;}


   void saveXML(xmlNode *root);

   virtual void newNetNotify(const string &cat, const string &type);

   void addTerminal(UINetTerminal *term);

   void removeTerminal(UINetTerminal *term);

   vector<string> getTerminals(UINetTerminal::NetTermType termType);

   void insertNetParams(vector<ItemInfo *> &params);

   virtual UINode *newNode(UINetwork* _net, xmlNodePtr def);
   
   virtual UINode *newNode(UINetwork* _net, string _name, 
						   string _type, double _x, double _y, bool doInit);

//   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to, char *str=NULL);


   /**
      Create a new UINote
      \param text the text of the note
      \param x the x position of the note (useful for GUINote)
      \param y the y position of the note (usfeul for GUINote)
      \param visible true if the note is visible (useful for GUINote)
      \return UINote *the pointer to the newly created note
   */
   virtual UINote* newNote(const std::string &text, double x, double y, bool visible);


   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const string &_name, 
					  const string &_objType="any", const string &_description="No description available");

   Network *build(const string &netName, const ParameterSet &params);

   /**Generate C++ code for building the document, instead of using XML*/
   void genCode(ostream &out, int &id, set<string> &nodeList);
 
   vector<UINode *> getNodes() {return nodes;}
   vector<UILink *> getLinks() {return links;}
   vector<UINetTerminal *> getTerminals() {return terminals;}

   ///Direct access to the note vector
   vector<UINote*> getNotes() {return m_notes;}

   void addNote(UINote *note);


   void removeNote(UINote *note);
   
   
   virtual void rename(string newName);

   void interfaceChangeNotify();

   virtual void updateAllSubnetTerminals(const string _nettype, const string _terminalname, 
					 UINetTerminal::NetTermType _terminaltype, bool _remove); 


   virtual void updateAllSubnetParameters(const string _nettype, NodeInfo* _info);

};


#endif
