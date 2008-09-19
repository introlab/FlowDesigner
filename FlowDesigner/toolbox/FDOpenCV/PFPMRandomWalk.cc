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

#include "PFPMRandomWalk.h"

using namespace std;
using namespace FD;

namespace RobotFlow {

DECLARE_NODE(PFPMRandomWalk)
DECLARE_TYPE(PFPMRandomWalk)

  /*Node
   *
   * @name PFPMRandomWalk
   * @category RobotFlow:Vision:Tracking
   * @description Random walk prediction model for particle filters.
   *
   * @input_name NOISE_VARIANCE
   * @input_type Vector<float>
   * @input_description Noise variance for each of the particle state.
   *
   * @input_name CURRENT_PARTICLE
   * @input_type PFGenericParticle
   * @input_description Current particle (sample) to apply prediction model.
   *
   * @output_name PREDICT_COMPLETED
   * @output_type bool
   * @output_description Flag indicating the success of the prediction.
   *
   END*/

PFPMRandomWalk::PFPMRandomWalk()
: PFPredictionModel(e_PFPM_RandomWalk, string("PFPMRandomWalk"), ParameterSet()),
m_init(false),
m_stateSize(0),
m_noiseVariance(NULL)
{ 
}
	
PFPMRandomWalk::PFPMRandomWalk(int i_stateSize, const Vector<float> *i_variance) 
: PFPredictionModel(e_PFPM_RandomWalk, string("PFPMRandomWalk"), ParameterSet()),
m_init(false),
m_stateSize(i_stateSize),
m_noiseVariance(NULL)
{ 
	Initialize(i_variance);
}
	
PFPMRandomWalk::PFPMRandomWalk(string nodeName, ParameterSet params)
: PFPredictionModel(e_PFPM_RandomWalk, nodeName, params),
m_init(false),
m_stateSize(0),
m_noiseVariance(NULL)
{
	SetType(e_PFPM_RandomWalk);
	
	m_varianceInID = addInput("NOISE_VARIANCE");
	m_particleInID = addInput("CURRENT_PARTICLE");
	
	m_completedOutID = addOutput("PREDICT_COMPLETED");
}

PFPMRandomWalk::~PFPMRandomWalk()
{
	if (m_noiseVariance) {
		m_noiseVariance->destroy();
	}
}

void PFPMRandomWalk::calculate(int output_id, int count, Buffer &out)
{
	try {
		// Get NOISE_VARIANCE only once for intialization
		if (!m_init) {
			ObjectRef varObjRef = getInput(m_varianceInID, count);
			
			if (!varObjRef->isNil()) {
				RCPtr<Vector<float> > varRef = varObjRef;
				
				Initialize(&(*varRef));
			}
			else {
				throw new GeneralException ("PFPMRandomWalk::calculate : invalid (nilObject) NOISE_VARIANCE input.",__FILE__,__LINE__);
			}
		}
		
		// Get current particle to apply prediction model to
		ObjectRef sampleObjRef = getInput(m_particleInID, count);
		
		if (!sampleObjRef->isNil()) {
			RCPtr<PFGenericParticle> sampleRef = sampleObjRef;
			
			// Particle is modified by reference
			Predict(sampleRef.get());
		}
		else {
			throw new GeneralException ("PFPMRandomWalk::calculate : invalid (nilObject) CURRENT_PARTICLE input.",__FILE__,__LINE__);
		}
		
		// Output true indicating prediction was correctly applied
		(*outputs[m_completedOutID].buffer)[count] = ObjectRef(Bool::alloc(true));
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in PFPMRandomWalk::calculate:",__FILE__,__LINE__));
	}
}

void PFPMRandomWalk::Predict(PFParticle *io_sample)
{
	try {
		float *p_state;
		
		// Check for current particle sanity
		if (m_stateSize != io_sample->GetStateSize()) {
			throw new GeneralException ("PFPMRandomWalk::Predict : current particle state size differs from variance vector size.",__FILE__,__LINE__);
		}
		
		p_state = io_sample->GetState();
		
		if (!p_state) {
			throw new GeneralException ("PFPMRandomWalk::Predict : current particle has an uninitialized state.",__FILE__,__LINE__);
		}
		
		for (int i=0; i<m_stateSize; i++) {
			*p_state = *p_state * (1.f + (PFUTIL_randn()-0.5f)*(*m_noiseVariance)[i]);
			p_state++;
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in PFPMRandomWalk::Predict:",__FILE__,__LINE__));
	}
}

void PFPMRandomWalk::Initialize(const Vector<float> *i_variance)
{
	try {
		m_stateSize = i_variance->size();
		m_noiseVariance = Vector<float>::alloc(m_stateSize);
		
		for (int i=0; i<m_stateSize; i++) {
			(*m_noiseVariance)[i] = (*i_variance)[i];
		}
		
		m_init = true;
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in PFPMRandomWalk::Initialize:",__FILE__,__LINE__));
	}
}

}
