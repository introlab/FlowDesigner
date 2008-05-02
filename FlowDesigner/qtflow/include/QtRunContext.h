#ifndef _QTRUNCONTEXT_H_
#define _QTRUNCONTEXT_H_

#include "UIDocument.h"
#include "UINodeParameters.h"
#include "ParameterSet.h"
#include "Network.h"
#include "QtProbeManager.h"

namespace FD 
{

	class QtRunContext
	{
		
		public:
		
		QtRunContext(UIDocument *doc, ParameterSet &params);
		~QtRunContext();
		
		bool run();
		
		private:
		
		UIDocument *m_document;
		Network  *m_network;
		ParameterSet m_parameters;
		QtProbeManager *m_probeManager;
	};

} //namespace FD

#endif
