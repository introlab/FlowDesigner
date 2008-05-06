// Copyright (C) 2007 Dominic Letourneau

#ifndef _UIPROBELINK_H_
#define _UIPROBELINK_H_

#include "UILink.h"
#include "UINode.h"
#include "Object.h"
#include "BufferedNode.h"
#include <list>
#include <map>

namespace FD {


	class UIObserverIF
	{
		public:

		virtual void notify (ObjectRef object, int count) = 0;
	};

	
	//WE HAVE CHOSEN A BUFFERED NODE TO AVOID
	//NOTIFYING EVERY TIME GETOUTPUT IS CALLED.
	//IS THIS THE RIGHT CHOICE ?
	class UIProbeLinkNode : public BufferedNode
	{
		protected:
		int m_inputID;
		int m_outputID;
	
		std::list<UIObserverIF*> m_observers;	
	
		public:
		
		void calculate(int output_id, int count, Buffer &out);
	
		void registerIF(UIObserverIF* client);
		
		void unregisterIF(UIObserverIF* client);
	
		UIProbeLinkNode(std::string nodeName, ParameterSet params);
	
	}; 

	

	/**
		UIProbeLink is a prototype, use at your own risk. The basic concept is to 
		provide an interface for graphical probes to be updated when an object is
		transited on the network. The concept is to use the pattern Observer-Observable
		to allow observers to receive the "notify" messages when the object is being transmitted.
		The implementation that we are trying to validate is based on UILink. The new link will in fact act like a single input, single output node and will replace the @old@ UILink in its functionalities.
	*/
	class UIProbeLink : public UILink
	{
		public:

 		UIProbeLink(UITerminal *_from, UITerminal *_to, const char *points_str=NULL);
	
		virtual ~UIProbeLink();

		virtual void saveXML(xmlNode *root);

   		virtual void build(Network *net);
   		
   		static std::map<UIProbeLink*, UIProbeLinkNode*> & getProbeDictionary();

		void registerIF(UIObserverIF* client);
		
		void unregisterIF(UIObserverIF* client);

		protected:

	};

} // namespace FD

#endif
