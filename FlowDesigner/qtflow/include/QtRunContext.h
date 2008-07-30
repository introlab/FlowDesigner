/***********************************************************************************
** Copyright (C) 2006-2008 Laborius (http://www.gel.usherbrooke.ca/laborius/). 
** All rights reserved. 
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License
** as published by the Free Software Foundation; either version 2
** of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
***********************************************************************************/
#ifndef _QTRUNCONTEXT_H_
#define _QTRUNCONTEXT_H_

#include "UIDocument.h"
#include "UINodeParameters.h"
#include "ParameterSet.h"
#include "Network.h"
#include "QtProbeManager.h"

namespace FD 
{

	class QtRunContext : public QObject
	{
		Q_OBJECT;
		
		public:
		
		QtRunContext(UIDocument *doc, ParameterSet &params);
		~QtRunContext();
		
		UIDocument* getDocument(){return m_document;}
		bool run();
		
		private:
		
		UIDocument *m_document;
		Network  *m_network;
		ParameterSet m_parameters;
		QtProbeManager *m_probeManager;
	};

} //namespace FD

#endif
