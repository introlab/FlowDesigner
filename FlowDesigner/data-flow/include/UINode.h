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
#include <list>

namespace FD {
	
	///Forward declarations
	class UINetwork;
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
	 @author Jean-Marc Valin, Dominic Letourneau
	 */
	class UINode {
		
	public:
		
		/**Observer interface, event aggregator
		 Default implementation will call global handler
		 */
		class UINodeObserverIF
		{
		public:
			//Global event for changes
			virtual void notifyChanged(const FD::UINode* node) {}
			
			//Terminal removed
			virtual void notifyTerminalRemoved(const FD::UINode *node, const FD::UITerminal* terminal) {notifyChanged(node);}
			
			//Terminal Added
			virtual void notifyTerminalAdded(const FD::UINode *node, const FD::UITerminal* terminal) {notifyChanged(node);}
			
			//Parameters changed
			virtual void notifyParametersChanged(const FD::UINode *node, const FD::UINodeParameters *params) {notifyChanged(node);}
			
			//Destroyed
			virtual void notifyDestroyed(const FD::UINode *node) {notifyChanged(node);}
			
			//Position changed
			virtual void notifyPositionChanged(const FD::UINode* node, double x, double y) {notifyChanged(node);}
			
			//Name changed
			virtual void notifyNameChanged(const FD::UINode* node, const std::string &name) {notifyChanged(node);}
			
		    //Type changed
			virtual void notifyTypeChanged(const FD::UINode* node, const std::string &type) {notifyChanged(node);}
			
			virtual ~UINodeObserverIF() {;}
		};
		
		
		
	protected:
		
		
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
		
		/**Pointers to all the inputs*/
		std::vector <UITerminal *> inputs;
		
		/**Pointers to all the outputs*/
		std::vector <UITerminal *> outputs;
		
		/** Event observers */
		std::list<UINodeObserverIF*> m_observers;
		
		/**All the node parameters*/
		UINodeParameters *parameters;
		
	public:
		
		/**"Normal" constructor*/
		UINode(UINetwork* _net, std::string _name, std::string _type, double x, double y);
		
		/**Constructor from XML parse tree*/
		UINode(UINetwork* _net, xmlNodePtr def);
		
		/**Destructor*/
		virtual ~UINode();
		
		/**Returns the node name*/
		const std::string &getName() {return name;}
		
		/**Returns the node type*/
		const std::string &getType() {return type;}
		
		/**
			Change the type of a node. This will occur when a subnet is inserted as a node.
			/param type The new type
		 */
		void setType(const std::string &type);
		
		/**Returns the corresponding network*/
		UINetwork * getNetwork() {return net;}
		
		/** Register an event observer */
		void registerEvents(UINodeObserverIF *observer);
		
		/** Unregister for events */
		void unregisterEvents(UINodeObserverIF *observer);
		
		/**Rename a node (when network included as a subnet)*/
		virtual void rename (const std::string &newName);
		
		/**Adds a new terminal to a node*/
		FD::UITerminal* addTerminal(const std::string &_name, UINetTerminal::NetTermType _type,
									const std::string &_objType="any", const std::string &_description="No description available");
		
		bool addTerminal(UITerminal *terminal);
		
		/**Removes a terminal from a node*/
		virtual void removeTerminal(const std::string &_name, UINetTerminal::NetTermType _type);
		
		virtual bool removeTerminal(UITerminal* terminal, bool deleteTerminal = true);
		
		/**Save to an XML parse tree*/
		void saveXML(xmlNode *root);
		
		/** Load from an XML tree */
		void loadXML(xmlNodePtr def);
		
		/**Returns the input (terminal) corresponding to a certain name*/
		UITerminal *getInputNamed(std::string n);
		
		/**Returns the output (terminal) corresponding to a certain name*/
		UITerminal *getOutputNamed(std::string n);
		
		/**Returns the node position*/
		void getPos (double &xx, double &yy);
		
		/**Changes the position (not too sure it should be used*/
		void setPos (double new_x, double new_y);
		
		void setNodeParameters(UINodeParameters *params);
		
		void insertNetParams(std::vector<ItemInfo *> &params);
		
		void updateNetParams(std::vector<ItemInfo *> &params);
		
		UITerminal* createTerminal(ItemInfo *_info, bool _isInput, double _x, double _y);
		
		virtual UINodeParameters *createNodeParameters (std::string type);
		
		Node *build(const ParameterSet &params);
		
		/**Generate C++ code for building the document, instead of using XML*/
		void genCode(std::ostream &out, int &id, std::set<std::string> &nodeList);
		
		std::vector<UITerminal *> getInputs();
		std::vector <UITerminal *> getOutputs();
		UINodeParameters * getParameters();
		std::string getDescription();
		std::string getComments() const;
		
		friend class UINetwork;
		
	};
	
}//namespace FD
#endif
