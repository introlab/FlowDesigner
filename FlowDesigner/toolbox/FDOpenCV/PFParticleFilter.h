/* Copyright (C) 2005 Pierre Moisan (Pierre.Moisan@USherbrooke.ca) 

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef _PFPARTICLEFILTER_H_
#define _PFPARTICLEFILTER_H_

#include "VisualTracker.h"
#include "PFParticle.h"
#include "PFUtilityFct.h"

namespace RobotFlow {

typedef enum
{
	e_PF_GenericFilter = 0,
	e_PF_Unknown
} e_PF_type;

//
// Abstract base class for particle filter
//

class PFParticleFilter : public VisualTracker
{
public:
	PFParticleFilter()
	: m_filterType(e_PF_Unknown)
	{
	
	}
	
	PFParticleFilter(e_PF_type i_modelType) 
	: m_filterType(i_modelType)
	{ 
	}
	
	PFParticleFilter(e_PF_type i_modelType, std::string nodeName, FD::ParameterSet params)
	: VisualTracker(nodeName, params),
	m_filterType(i_modelType)
	{
	
	}

	virtual ~PFParticleFilter()
	{
	
	}

	// Default routine to print a PFParticleFilter object to an output stream
	void printOn(std::ostream &out) const = 0;

	// Default routine to read a PFParticleFilter object from an input stream
	void readFrom(std::istream &in) = 0;
	
	void calculate(int output_id, int count, FD::Buffer &out) = 0;
	
	e_PF_type GetType() const
	{ 
		return m_filterType; 
	}
	
	void SetType(e_PF_type i_type)
	{
		m_filterType = i_type;
	}

private:
	e_PF_type m_filterType;
};

}

#endif
