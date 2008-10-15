// Copyright (C) 2001 Jean-Marc Valin

#ifndef UIDOCUMENT_H
#define UIDOCUMENT_H

#include <vector>
#include <string>
#include <libxml/tree.h>
#include <map>
#include <list>
#include "UINetwork.h"
#include "UINodeParameters.h"
#include <set>
#include "Object.h"
#include "UINodeRepository.h"

namespace FD {

class UINodeMenu;
class Network;
class ParameterSet;

/**
   GUI-independent memory representation for an XML FlowDesigner document
   @author: Jean-Marc Valin
*/
class UIDocument : public Object {
	
public:
	static const int DEFAULT_CONNECTION_PORT = 0; // if 0, a port is chosen automatically 
	
	class UIDocumentObserverIF
	{
		public:
			//Global event for changes
			virtual void notifyChanged(const FD::UIDocument* doc) {}
			
			//Network removed
			virtual void notifyNetworkRemoved(const FD::UIDocument *doc, const FD::UINetwork* net) {notifyChanged(doc);}
			
			//Network Added
			virtual void notifyNetworkAdded(const FD::UIDocument *doc, const FD::UINetwork* net) {notifyChanged(doc);}
						
			//Parameters changed
			virtual void notifyParametersChanged(const FD::UIDocument *doc, const FD::ItemInfo *param) {notifyChanged(doc);}
			
			//Name changed
			virtual void notifyNameChanged(const FD::UIDocument *doc, const std::string &name){notifyChanged(doc);}
			
			//Path changed
			virtual void notifyPathChanged(const FD::UIDocument *doc, const std::string path){notifyChanged(doc);}
			
			//Category changed
			virtual void notifyCategoryChanged(const FD::UIDocument *doc, const std::string &category){notifyChanged(doc);}
			
			//Comments changed
			virtual void notifyCommentsChanged(const FD::UIDocument *doc, const std::string &comments){notifyChanged(doc);}
						
			//Destroyed
			virtual void notifyDestroyed(const FD::UIDocument *doc) {notifyChanged(doc);}	
		
			virtual ~UIDocumentObserverIF(){;}
	};
	
   
protected:
	
   /**Pointers to all networks included (defined) in the document*/
   std::vector<UINetwork *> networks;

   /**Flag is set to true anytime there's a change that hasn't been saved*/
   bool modified;
   
   /**Flag is set to true if the document can be modified*/
   bool editable;
   
   /**Info about all internal networks that can be included as nodes - I think (should be cleaned up)*/
   UINodeRepository subnetInfo;

   /**Document parameters (should be re-written)*/
   std::vector<ItemInfo *> textParams;

   /**Input for the document if used as an external node*/
   std::vector<ItemInfo *> docInputs;

   /**Output for the document if used as an external node*/
   std::vector<ItemInfo *> docOutputs;

   /**Parameters for the document if used as an external node*/
   std::vector<ItemInfo *> docParams;

   /**File name (no path)*/
   std::string docName;
   
   /**Document path*/
   std::string path;
   
   /**True if document has no real name yet*/
   bool untitled;

   /**True if the object has already been destroyed (by superclass destructor)*/
   bool destroyed;

   /**Category of the document, used when the document's MAIN network is to be inserted as a node */
   std::string category;

   /**Comments inserted in the document */
   std::string m_comments;
   
   std::list<UIDocumentObserverIF*> m_observers;
   
   /**Port used for remote connection and for probing*/
   int m_connectionPort;

public:
   /**Document constructor with name, DOES NOT load the document (document created as untitled)*/
   UIDocument(std::string _name);

   /**Virtual destructor*/
   virtual ~UIDocument();

   /**Explicitly loads a document using the name specified at construction*/
   virtual void load();

   /**Loads the document from a parsed XML tree*/
   virtual void loadXML(xmlNodePtr root);

   /**Loads the document from memory*/
   virtual void loadFromMemory(const char *mem, int size);
   
   /**Set the category of the document in the node menu */
   void setCategory(const std::string &cat);

   /**Get the category of the document in the node menu */
   const std::string &getCategory();

   /**Set comments for the document */
   void setComments(const std::string &comments);

   /**Get comments from the document */
   const std::string &getComments();
   
   /**Sets the 'modified' flag*/
   void setModified(bool flag);
   
   /**Has the document been modified since last saved?*/
   bool isModified() {return modified;}
   
   /**Set the 'editable' flag*/
   void setEditable(bool flag) {editable = flag;}
   
   /**Is the document editable?*/
   bool isEditable() {return editable;}
   
   /**Get the connection port*/
   int getConnectionPort() const {return m_connectionPort;}
   
   /**Set the connection port*/
   void setConnectionPort(int port) {m_connectionPort = port;}
   
   /**Add a network to the document*/
   UINetwork *addNetwork(std::string name, UINetwork::Type type);
   
   /**Add a network to the document from an XML node (used in load())*/
   UINetwork *addNetwork(xmlNodePtr xmlNet);
   
   /**Remove and destroy network from the document*/
   void removeNetwork(UINetwork *toRemove, bool deleteNetwork=true);
   
   /**Is the document untitled?*/
   bool isUntitled() {return untitled;}
   
   /**Document name (no path*/
   virtual const std::string &getName() {return docName;}
   
   /**Document path*/
   const std::string &getPath() {return path;}
   
   /**Save the document to file specified in internal variables 'path' and 'docName'*/
   virtual void save();
   
   /**Convert document to an XML string in memory*/
   char *saveToMemory(int &size);
   
   void addParameterText(const std::string &name, const std::string &value, const std::string &type, const std::string &description);
   
   UINetwork *getNetworkNamed(const std::string &n);
   
   std::vector<ItemInfo *> getNetInputs(const std::string &netName);
   
   std::vector<ItemInfo *> getNetOutputs(const std::string &netName);
   
   std::vector<ItemInfo *> getNetParams(const std::string &netName);
   
   std::string getDescription(const std::string &type);
   
   /**A UIDocument can print itself*/
   void printOn(std::ostream &out=std::cout) const;

   std::vector<UINetwork *> get_networks();

   std::vector<ItemInfo *> get_textParams();


   
   Network *build(const std::string &_name, const ParameterSet &params);

   /**Generate C++ code for building the document, instead of using XML*/
   std::set<std::string> genCode(std::ostream &out, const std::string &functName, bool localIncludes=false);

   /** Export a network into XML (for future import) 
      @author Dominic Letourneau (August 9 2004)
      \param networkName The name of the network to export
      \param fileName The name of the file to create, will be in XML format
   */
   void exportNetwork(const std::string &networkName, const std::string &fileName);

   
   /** Import a network from XML file
       @author Dominic Letourneau (August 9 2004)
       \param fileName The file containing the exported XML network
   */
   void importNetwork(const std::string &fileName);


   static Network *buildExternal(const std::string &type, const std::string &_name, const ParameterSet &params);

   virtual void run();

   virtual void run(ParameterSet &p);

   virtual void setFullPath(const std::string &fullpath);

   void updateNetInfo(UINetwork *net); 

   void updateAllNetworks();
   
   static std::string findExternal(const std::string &filename, const char *searchPath="FLOWDESIGNER_PATH", bool include_home=true, bool fullPathOutput=true);

   static void genCodeExternal(const std::string &type, std::ostream &out, int &id, std::set<std::string> &nodeList);

   virtual void updateAllSubnetTerminals(const std::string _nettype, const std::string _terminalname, 
					 UINetTerminal::NetTermType _terminaltype, bool _remove); 

   UINodeRepository &getRepository();
   
   /** Register an event observer */
   void registerEvents(UIDocumentObserverIF *observer);
   
   /** Unregister for events */
   void unregisterEvents(UIDocumentObserverIF *observer);
   
   
 protected:
   
   virtual void error(const char *err);
	
   virtual UINetwork *newNetwork(const std::string &_name, UINetwork::Type type);
	
   virtual UINetwork *newNetwork(xmlNodePtr _net);

 private:

   static bool findExternalRecursive(const std::string &basePath, const std::string &path, const std::string &type, std::string &fullname, bool fullPathOutput);
};


}//namespace FD

#endif
