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

#ifndef _PFPMRANDOMWALK_H_
#define _PFPMRANDOMWALK_H_

#include "PFPredictionModel.h"
#include "PFGenericParticle.h"
#include "PFUtilityFct.h"
#include "Vector.h"

namespace RobotFlow {
//
// Random walk prediction model for particle filters
//
class PFPMRandomWalk : public PFPredictionModel
{
public:
	PFPMRandomWalk();
	
	PFPMRandomWalk(int i_stateSize, const FD::Vector<float> *i_variance);
	
	PFPMRandomWalk(std::string nodeName, FD::ParameterSet params);

	virtual ~PFPMRandomWalk();
	
	virtual void calculate(int output_id, int count, FD::Buffer &out);
	
	virtual void Predict(PFParticle *io_sample);
	
	void Initialize(const FD::Vector<float> *i_variance);
	
private:
	// BufferedNode inputs
	int m_varianceInID;
	int m_particleInID;
	
	// BufferedNode outputs
	int m_completedOutID;
	
	bool m_init;
	unsigned int m_stateSize;
	FD::Vector<float> *m_noiseVariance;
};

}

#endif
