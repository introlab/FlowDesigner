// Copyright (C) 2001 Jean-Marc Valin

#ifndef UIDOCUMENT_H
#define UIDOCUMENT_H

#include <vector>
#include <string>
#include <libxml/tree.h>
#include <map>
#include "UINetwork.h"
#include "UINodeParameters.h"
#include <set>
#include "Object.h"
#include "UINodeRepository.h"

namespace FD {

class UINodeMenu;
class Network;
class ParameterSet;

//FIXME: Should replace with ItemInfo
class DocParameterDataText {
  public:
   std::string name;
   std::string value;
   std::string type;
};

/**
   GUI-independent memory representation for an XML FlowDesigner document
   @author: Jean-Marc Valin
*/
class UIDocument : public Object {
   
protected:
   /**Pointers to all networks included (defined) in the document*/
   std::vector<UINetwork *> networks;

   /**Flag is set to true anytime there's a change that hasn't been saved*/
   bool modified;
   
   /**Info about all internal networks that can be included as nodes - I think (should be cleaned up)*/
   UINodeRepository subnetInfo;

   /**Document parameters (should be re-written)*/
   std::vector<DocParameterDataText *> textParams;

   /**Input for the document if used as an external node*/
   std::vector<ParameterText *> docInputs;

   /**Output for the document if used as an external node*/
   std::vector<ParameterText *> docOutputs;

   /**Parameters for the document if used as an external node*/
   std::vector<ParameterText *> docParams;

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
   void setCategory(const std::string &cat) {category = cat;}

   /**Get the category of the document in the node menu */
   const std::string &getCategory() {return category;}

   /**Set comments for the document */
   void setComments(const std::string &comments) {m_comments = comments;}

   /**Get comments from the document */
   const std::string &getComments() {return m_comments;}
   
   /**Sets the 'modified' flag*/
   void setModified() {modified=true;}
   
   /**Resets the 'modified' flag*/
   void resetModified() {modified=false;}
   
   /**Has the document been modified since last saved?*/
   bool isModified() {return modified;}
   
   /**Add a network to the document*/
   UINetwork *addNetwork(std::string name, UINetwork::Type type);
   
   /**Add a network to the document from an XML node (used in load())*/
   UINetwork *addNetwork(xmlNodePtr xmlNet);
   
   /**Remove and destroy network from the document*/
   void removeNetwork(UINetwork *toRemove);
   
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
   
   void addParameterText(std::string name, std::string value, std::string type);
   
   UINetwork *getNetworkNamed(const std::string &n);
   
   std::vector<ItemInfo *> getNetInputs(const std::string &netName);
   
   std::vector<ItemInfo *> getNetOutputs(const std::string &netName);
   
   std::vector<ItemInfo *> getNetParams(const std::string &netName);
   
   std::string getDescription(const std::string &type);
   
   /**A UIDocument can print itself*/
   void printOn(std::ostream &out=std::cout) const;

   std::vector<UINetwork *> get_networks() {return networks;}

   std::vector<DocParameterDataText *> get_textParams() {return textParams;}

   virtual UINetwork *newNetwork(const std::string &_name, UINetwork::Type type);
   
   virtual UINetwork *newNetwork(xmlNodePtr _net);
   
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
   
   static std::string findExternal(const std::string &filename, char *searchPath="FLOWDESIGNER_PATH", bool include_home=true, bool fullPathOutput=true);

   static void genCodeExternal(const std::string &type, std::ostream &out, int &id, std::set<std::string> &nodeList);

   virtual void updateAllSubnetTerminals(const std::string _nettype, const std::string _terminalname, 
					 UINetTerminal::NetTermType _terminaltype, bool _remove); 

   UINodeRepository &getRepository() {return subnetInfo;}
   
 protected:
   
   virtual void error(char *err);

 private:

   static bool findExternalRecursive(const std::string &basePath, const std::string &path, const std::string &type, std::string &fullname, bool fullPathOutput);
};


}//namespace FD

#endif
