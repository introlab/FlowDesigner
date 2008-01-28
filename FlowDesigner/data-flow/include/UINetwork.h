// Copyright (C) 2001 Jean-Marc Valin

#ifndef UINETWORK_H
#define UINETWORK_H

//#include <gnome.h>
//#include "UIDocument.h"
#include <vector>
#include <string>
#include <libxml/tree.h>
#include <fstream>
#include "UINetTerminal.h"
#include "UINodeParameters.h"
#include "UINodeRepository.h"
#include <set>
#include <list>

namespace FD {
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
    @author Jean-Marc Valin, Dominic Letourneau
*/
class UINetwork {
  public:
	  
	class UINetworkObserverIF
	{
		public:
			
		//Global event for changes
		virtual void notifyChanged(const UINetwork* net) {}
		
		//Node removed
		virtual void notifyNodeRemoved(const UINetwork *net, const UINode* node) {notifyChanged(net);}
		
		//Node added
		virtual void notifyNodeAdded(const UINetwork *net, const UINode* node) {notifyChanged(net);}
		
		//Link removed
		virtual void notifyLinkRemoved(const UINetwork *net, const UILink* link) {notifyChanged(net);}
		
		//Link added
		virtual void notifyLinkAdded(const UINetwork *net, const UILink* link) {notifyChanged(net);}

		//Note removed
		virtual void notifyNoteRemoved(const UINetwork *net, const UINote* note) {notifyChanged(net);}
		
		//Note added
		virtual void notifyNoteAdded(const UINetwork *net, const UINote* note) {notifyChanged(net);}
		
		//NetTerminal removed
		virtual void notifyNetTerminalRemoved(const UINetwork *net, const UINetTerminal* terminal) {notifyChanged(net);}
		
		//NetTerminal added
		virtual void notifyNetTerminalAdded(const UINetwork *net, const UINetTerminal* terminal) {notifyChanged(net);}
		
		//Name changed
		virtual void notifyNameChanged(const UINetwork *net, const std::string &name) {notifyChanged(net);}
		
		//Description changed
		virtual void notifyDescriptionChanged(const UINetwork *net, const std::string &description) {notifyChanged(net);}
		
		//Destroyed
		virtual void notifyDestroyed(const UINetwork *net) {notifyChanged(net);}
	};
	  
	  
	  
   /**Available network types*/
   enum Type {subnet=0, iterator=1, threaded=2};
protected:

   /**Has the network been destroyed? Not too sure what I was thinking...*/
   bool destroyed;

   /**Document (.n file) to which the network belongs*/
   UIDocument *doc;

   /**Name of the network*/
   std::string name;

   /**Description of the network*/
   std::string m_description;

   /**Network type (subnet, iterator, threaded iterator*/
   Type type;

   /**All the links included in the network*/
   std::vector <UINode *> nodes;

   /**All the links included in the network*/
   std::vector <UILink *> links;

   /**Pointer to all the network terminals*/
   std::vector <UINetTerminal *> terminals;

   /**All the notes in the network*/
   std::vector <UINote*> m_notes;
   
   /** List of network observers */
   std::list<UINetworkObserverIF*> m_observers;

   ///The condition node of the iterator (no meaning for subnets)
   //UINode *conditionNode;
private:
   
   /**Used to determine infinite recursion in build*/
   bool buildRecurs;
public:
   /**Basic constructor, allows building the UINetwork gradually*/
   UINetwork(UIDocument *_doc, std::string _name, Type _type);

   /**Construct a UINetwork from a parsed XML file*/
   UINetwork(UIDocument *_doc, xmlNodePtr net, bool init=true);

   /**Loads the info from the XML parse tree*/
   void load (xmlNodePtr net);

   /**Destructor*/
   virtual ~UINetwork();

   /**Informs that some part of the network has been changed. To be called anytime one modifies something in the network, a node, a link, ...*/
   void setModified();

   UINode *loadNode (xmlNodePtr node);
   
   UINode *getNodeNamed(std::string n);
   
   void addNode(UINode *node);

   void removeNode (UINode *node);

   void addLink (UILink *link);

   void removeLink (UILink *link);

   std::string getName() const;

   std::string getDescription() const;

   void setDescription(const std::string & description);

   Type getType() const;

   UIDocument *getDocument();

   bool isIter();


   void saveXML(xmlNode *root);

   virtual void newNetNotify(const std::string &cat, const std::string &type);

   void addTerminal(UINetTerminal *term);

   void removeTerminal(UINetTerminal *term);

   std::vector<std::string> getTerminals(UINetTerminal::NetTermType termType);

   void insertNetParams(std::vector<ItemInfo *> &params);

   virtual UINode *newNode(UINetwork* _net, xmlNodePtr def);
   
   virtual UINode *newNode(UINetwork* _net, std::string _name, 
						   std::string _type, double _x, double _y, bool doInit);

//   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to,const char *str=NULL);


   /**
      Create a new UINote
      \param label the label of the note
      \param text the text of the note
      \param x the x position of the note (useful for GUINote)
      \param y the y position of the note (usfeul for GUINote)
      \param visible true if the note is visible (useful for GUINote)
      \return UINote *the pointer to the newly created note
   */
   virtual UINote* newNote(const std::string &label, const std::string &text, double x, double y, bool visible);


   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name, 
					  const std::string &_objType="any", const std::string &_description="No description available");

   Network *build(const std::string &netName, const ParameterSet &params);

   /**Generate C++ code for building the document, instead of using XML*/
   void genCode(std::ostream &out, int &id, std::set<std::string> &nodeList);
 
   std::vector<UINode *> getNodes();
   
   std::vector<UILink *> getLinks();
   
   std::vector<UINetTerminal *> getTerminals();

   ///Direct access to the note vector
   std::vector<UINote*> getNotes();
   
   void addNote(UINote *note);

   void removeNote(UINote *note);
   
   virtual void rename(std::string newName);

   void interfaceChangeNotify();

   virtual void updateAllSubnetTerminals(const std::string _nettype, const std::string _terminalname, 
					 UINetTerminal::NetTermType _terminaltype, bool _remove); 


   virtual void updateAllSubnetParameters(const std::string _nettype, NodeInfo* _info);
   
   /** Register an event observer */
   void registerEvents(UINetworkObserverIF *observer);
   
   /** Unregister for events */
   void unregisterEvents(UINetworkObserverIF *observer);

};

}//namespace FD
#endif
