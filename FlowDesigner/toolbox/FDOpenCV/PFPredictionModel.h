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

#ifndef _PFPREDICTIONMODEL_H_
#define _PFPREDICTIONMODEL_H_

#include "BufferedNode.h"
#include "PFParticle.h"

namespace RobotFlow {

typedef enum
{
	e_PFPM_RandomWalk = 0,
	e_PFPM_Unknown
} e_PFPM_type;

//
// Abstract base class for particle filter prediction models
//
class PFPredictionModel : public FD::BufferedNode
{
public:
	PFPredictionModel()
	: m_modelType(e_PFPM_Unknown)
	{ 
	}
	
	PFPredictionModel(e_PFPM_type i_modelType) 
	: m_modelType(i_modelType)
	{ 
	}
	
	PFPredictionModel(e_PFPM_type i_modelType, std::string nodeName, FD::ParameterSet params)
	: FD::BufferedNode(nodeName, params),
	m_modelType(i_modelType)
	{
	
	}

	virtual ~PFPredictionModel()
	{
	
	}
	
	e_PFPM_type GetType() const
	{ 
		return m_modelType; 
	}
	
	void SetType(e_PFPM_type i_type)
	{
		m_modelType = i_type;
	}
	
	virtual void calculate(int output_id, int count, FD::Buffer &out) = 0;
	
	virtual void Predict(PFParticle *io_sample) = 0;
	
private:
	e_PFPM_type m_modelType;
};

}

#endif
