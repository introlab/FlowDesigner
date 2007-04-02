// Copyright (C) 2007 Dominic Letourneau

#ifndef _UIPROBELINK_H_
#define _UIPROBELINK_H_

#include "UILink.h"
#include "UINode.h"
#include "Object.h"
#include <vector>

namespace FD {


	class UIObserverIF
	{
		public:

		virtual void notify (ObjectRef object) = 0;
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

		void registerIF(UIObserverIF* client);

		protected:
		
		std::vector<UIObserverIF*> m_observers;
	};

} // namespace FD

#endif
