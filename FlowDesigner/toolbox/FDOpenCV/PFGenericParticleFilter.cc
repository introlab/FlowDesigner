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

#include "PFGenericParticleFilter.h"

using namespace std;
using namespace FD;

namespace RobotFlow {

DECLARE_NODE(PFGenericParticleFilter)

  /*Node
   *
   * @name PFGenericParticleFilter
   * @category RobotFlow:Vision:Tracking
   * @description Generic implementation of a particle filter.
   *
   * @parameter_name NUM_PARTICLES
   * @parameter_type int
   * @parameter_value 200
   * @parameter_description Number of particles (samples) to use.
   *
   * @parameter_name PARTICLE_STATE_SIZE
   * @parameter_type int
   * @parameter_value 4
   * @parameter_description Particle state size.
   *
   * @input_name INIT_VARIANCE
   * @input_type Vector<float>
   * @input_description Variance for initialization of each of the particle state.
   *
   * @input_name REFERENCE_MEAN_STATE
   * @input_type PFGenericParticle
   * @input_description Current reference mean state to track.
   *
   * @input_name PREDICT_COMPLETED
   * @input_type bool
   * @input_description Flag indicating the success of the prediction.
   *
   * @input_name PARTICLE_LIKELIHOOD
   * @input_type float
   * @input_description Resulting measurement likelihood for current particle.
   *
   * @output_name FILTERING_FINISHED
   * @output_type bool
   * @output_description Flag indicating that the tracking has completed.
   *
   * @output_name CURRENT_PARTICLE
   * @output_type PFGenericParticle
   * @output_description Current particle (sample) to apply prediction model.
   *
   * @output_name TRACKED_MEAN_STATE
   * @output_type PFGenericParticle
   * @output_description Resulting mean state after particle filtering.
   *
   END*/

PFGenericParticleFilter::PFGenericParticleFilter()
: PFParticleFilter(e_PF_GenericFilter, string("PFGenericParticleFilter"), ParameterSet()),
m_init(false),
m_initPF(false),
m_initSamples(false),
m_finished(false),
m_curSampleIdx(-1),
m_numSamples(0),
m_sampleStateSize(0),
m_samples(NULL),
m_tmpSamples(NULL),
m_cumulWeight(NULL),
m_initVariance(NULL),
m_curSample(NULL),
m_refMeanState(NULL),
m_outMeanState(NULL)
{ 
}
	
PFGenericParticleFilter::PFGenericParticleFilter(unsigned int i_numSamples, 
		unsigned int i_sampleStateSize, const Vector<float> *i_initVariance) 
: PFParticleFilter(e_PF_GenericFilter, string("PFGenericParticleFilter"), ParameterSet()),
m_init(false),
m_initPF(false),
m_initSamples(false),
m_finished(false),
m_curSampleIdx(-1),
m_numSamples(i_numSamples),
m_sampleStateSize(i_sampleStateSize),
m_samples(NULL),
m_tmpSamples(NULL),
m_cumulWeight(NULL),
m_initVariance(NULL),
m_curSample(NULL),
m_refMeanState(NULL),
m_outMeanState(NULL)
{ 
	Initialize(i_initVariance);
}
	
PFGenericParticleFilter::PFGenericParticleFilter(string nodeName, ParameterSet params)
: PFParticleFilter(e_PF_GenericFilter, nodeName, params),
m_init(false),
m_initPF(false),
m_initSamples(false),
m_finished(false),
m_curSampleIdx(-1),
m_numSamples(0),
m_sampleStateSize(0),
m_samples(NULL),
m_tmpSamples(NULL),
m_cumulWeight(NULL),
m_initVariance(NULL),
m_curSample(NULL),
m_refMeanState(NULL),
m_outMeanState(NULL)
{
	m_initVarianceInID = addInput("INIT_VARIANCE");
	m_refMeanStateInID = addInput("REFERENCE_MEAN_STATE");
	m_predictInID = addInput("PREDICT_COMPLETED");
	m_likelihoodInID = addInput("PARTICLE_LIKELIHOOD");
	
	m_finishedOutID = addOutput("FILTERING_FINISHED");
	m_particleOutID = addOutput("CURRENT_PARTICLE");
	m_meanStateOutID = addOutput("TRACKED_MEAN_STATE");
	
	m_numSamples = dereference_cast<int>(parameters.get("NUM_PARTICLES"));
	m_sampleStateSize = dereference_cast<int>(parameters.get("PARTICLE_STATE_SIZE"));
}

PFGenericParticleFilter::~PFGenericParticleFilter()
{
	if (m_initVariance) {
		m_initVariance->destroy();
	}
	
	delete [] m_samples;
	delete [] m_tmpSamples;
	delete [] m_cumulWeight;
}

// Modified BufferedNode request method to support cyclic node connection
void PFGenericParticleFilter::request(int output_id, const ParameterSet &req) 
{
	if (req.exist("LOOKAHEAD")) {
		outputs[output_id].lookAhead = max(outputs[output_id].lookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
	}
	
	if (req.exist("LOOKBACK")) {
		outputs[output_id].lookBack = max(outputs[output_id].lookBack,dereference_cast<int> (req.get("LOOKBACK")));
	}
	
	if (req.exist("INORDER")) {
		inOrder = true;
	}
	
	int outputLookAhead=0, outputLookBack=0;

	outputLookAhead=max(outputLookAhead, outputs[output_id].lookAhead);
	outputLookBack =max(outputLookBack, outputs[output_id].lookBack);
	
	if (output_id == m_finishedOutID) {
		// TRACKING_FINISHED output does not required any inputs
		return;
		
	}
	else if (output_id == m_particleOutID) {
		// CURRENT_PARTICLE output does not required any inputs
		return;
	}
	else if (output_id == m_meanStateOutID) {
		ParameterSet myReq, myReq2, myReq3, myReq4;
		
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_initVarianceInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_initVarianceInID].lookBack+outputLookBack)));
		inputs[m_initVarianceInID].node->request(inputs[m_initVarianceInID].outputID,myReq);
		
		myReq2.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_refMeanStateInID].lookAhead+outputLookAhead)));
		myReq2.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_refMeanStateInID].lookBack+outputLookBack)));
		inputs[m_refMeanStateInID].node->request(inputs[m_refMeanStateInID].outputID,myReq2);
		
		myReq3.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_predictInID].lookAhead+outputLookAhead)));
		myReq3.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_predictInID].lookBack+outputLookBack)));
		inputs[m_predictInID].node->request(inputs[m_predictInID].outputID,myReq3);
		
		myReq4.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_likelihoodInID].lookAhead+outputLookAhead)));
		myReq4.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_likelihoodInID].lookBack+outputLookBack)));
		inputs[m_likelihoodInID].node->request(inputs[m_likelihoodInID].outputID,myReq4);
	}
	else {
		throw new GeneralException ("PFGenericParticleFilter::request : unknown output ID.",__FILE__,__LINE__);
	}
}


void PFGenericParticleFilter::calculate(int output_id, int count, Buffer &out)
{
	try {
		if (output_id == m_finishedOutID) {
			(*outputs[m_finishedOutID].buffer)[count] = ObjectRef(Bool::alloc(!m_finished));
			if (m_finished) {
				m_initPF = false;
				m_finished = false;
			}
		}
		else if (output_id == m_particleOutID) {
			(*outputs[m_particleOutID].buffer)[count] = ObjectRef(m_curSample);
		}
		else if (output_id == m_meanStateOutID){
			if (!m_initPF) {
				// First get reference mean state to track
				ObjectRef meanStateRef = getInput(m_refMeanStateInID, count);
				
				if (meanStateRef->isNil()) {
					// Tracking failed of has not yet started
					m_initSamples = false;
					
					// Invalid target, output nilObject
					(*outputs[m_meanStateOutID].buffer)[count] = ObjectRef(nilObject);
					return;
				}
				
				m_refMeanState = RCPtr<PFGenericParticle>(meanStateRef);
				
				// Get INIT_VARIANCE only once for intialization
				if (!m_init) {
					ObjectRef varObjRef = getInput(m_initVarianceInID, count);
					
					if (!varObjRef->isNil()) {
						RCPtr<Vector<float> > varRef = varObjRef;
						
						Initialize(&(*varRef));
					}
					else {
						throw new GeneralException ("PFGenericParticleFilter::calculate : invalid (nilObject) INIT_VARIANCE input.",__FILE__,__LINE__);
					}
				}
				
				if (!m_initSamples) {
					InitSamples();
				}
				
				m_curSampleIdx = 0;
				m_likelihoodsSum = 0.f;
				
				m_initPF = true;
			}
			
			// Get a reference to current particle
			m_curSample = RCPtr<PFGenericParticle>(m_samples[m_curSampleIdx]);
			
			//
			// Prediction step
			// Apply prediction to current particle
			//
			bool predictCompleted = dereference_cast<bool>(getInput(m_predictInID, count));
			
			if (!predictCompleted) {
				throw new GeneralException ("PFGenericParticleFilter::calculate : prediction failed on current particle.",__FILE__,__LINE__);
			}
			
			//
			// Measurement step
			// Compute likelihood of the current particle
			//
			float likelihood = dereference_cast<float>(getInput(m_likelihoodInID, count));
			
			if (likelihood < 0.f || likelihood > 1.f) {
				throw new GeneralException ("PFGenericParticleFilter::calculate : invalid likelihood for current particle (should be between [0.0, 1.0]).",__FILE__,__LINE__);
			}
			
			// Assign likelihood to current particle's weight
			m_curSample->SetWeight(likelihood);
			m_likelihoodsSum += likelihood;
			
			/*
			cout << "Current particle info: likelihood=" << likelihood << endl;
			float *p_refState = m_refMeanState->GetState();
			for (int i=0; i<m_sampleStateSize; i++) {
				cout << "State[" << i << "]=" << *p_refState++ << endl;
			}
			*/
			
			m_curSampleIdx++;
			
			// Verify if we have done every particle
			if (m_curSampleIdx >= m_numSamples) {
				m_finished = true;
			}
				
			if (!m_finished) {
				(*outputs[m_meanStateOutID].buffer)[count] = ObjectRef(nilObject);
			}
			else {
				//
				// Update step
				// Propagate densities and update samples
				//
				Update();
				
				// Compute mean state
				ComputeMeanState();
				
				//
				// Resampling step
				//
				Resample();
				
				// Output mean state
				(*outputs[m_meanStateOutID].buffer)[count] = ObjectRef(m_outMeanState);
			}
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in PFGenericParticleFilter::calculate:",__FILE__,__LINE__));
	}
}

void PFGenericParticleFilter::Initialize(const Vector<float> *i_variance)
{
	try {
		if (m_sampleStateSize != i_variance->size()) {
			throw new GeneralException ("PFGenericParticleFilter::Initialize : particle state size differs from variance vector size.",__FILE__,__LINE__);
		}
		m_initVariance = Vector<float>::alloc(m_sampleStateSize);
		
		for (int i=0; i<m_sampleStateSize; i++) {
			(*m_initVariance)[i] = (*i_variance)[i];
		}
		
		m_samples = new RCPtr<PFGenericParticle>[m_numSamples];
		m_tmpSamples = new RCPtr<PFGenericParticle>[m_numSamples];
		
		for (int s=0; s<m_numSamples; s++) {
			m_samples[s] = RCPtr<PFGenericParticle>(new PFGenericParticle(m_sampleStateSize));
			m_tmpSamples[s] = RCPtr<PFGenericParticle>(new PFGenericParticle(m_sampleStateSize));
		}
		
		m_cumulWeight = new float[m_numSamples];
		
		m_outMeanState = RCPtr<PFGenericParticle>(new PFGenericParticle(m_sampleStateSize));
		
		m_init = true;
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in PFGenericParticleFilter::Initialize:",__FILE__,__LINE__));
	}
}

void PFGenericParticleFilter::InitSamples()
{
	int i, s;
	float *p_sampleState, *p_refState;
	
	if (!m_samples) {
		throw new GeneralException ("PFGenericParticleFilter::InitSamples : impossible to initialize samples without proper memory allocation.",__FILE__,__LINE__);
	}
	
	if (m_refMeanState->GetType() == e_PFP_Unknown) {
		throw new GeneralException ("PFGenericParticleFilter::InitSamples : cannot use particle with unknown type.",__FILE__,__LINE__);
	}
	
	for (s=0; s<m_numSamples; s++) {
		p_sampleState = m_samples[s]->GetState();
		p_refState = m_refMeanState->GetState();
		
		for (i=0; i<m_sampleStateSize; i++) {
			*p_sampleState++ = (*p_refState++)*(1.f + (*m_initVariance)[i]*(PFUTIL_randn()-0.5f));
		}
	}
	
	m_initSamples = true;
}

void PFGenericParticleFilter::Update()
{
	int s;
	float *p_cumulWeight, curWeight;
	float cumulSum=1e-30, weightSum=1e-30, weightSumInv, weightBias;
	
	if (!m_samples) {
		throw new GeneralException ("PFGenericParticleFilter::Update : impossible to update with uninitialized samples.",__FILE__,__LINE__);
	}
	
	p_cumulWeight = m_cumulWeight;
	
	/*
	if (m_likelihoodsSum != 0.f) {
		likelihoodsSumInv = 1.f/m_likelihoodsSum;
	}
	else {
		likelihoodsSumInv = 1.f;
	}
	*/
	
	for (s=0; s<m_numSamples; s++) {
		// Normalize weights
		//curWeight = m_samples[s]->GetWeight()*likelihoodsSumInv;
		curWeight = m_samples[s]->GetWeight();
		m_samples[s]->SetWeight(curWeight);
		cumulSum += curWeight;
		
		// Compute cumulative weights
		*p_cumulWeight++ = cumulSum;
		weightSum += cumulSum;
	}
	
	weightSumInv = 1.f/weightSum;
	weightBias = (1.f-m_likelihoodsSum)/m_numSamples;
	
	p_cumulWeight = m_cumulWeight;
	
	for (s=0; s<m_numSamples; s++) {
		*p_cumulWeight = weightSumInv*(*p_cumulWeight)*m_likelihoodsSum + weightBias;
		p_cumulWeight++;
	}
	
	p_cumulWeight = m_cumulWeight;
	weightSum = 1e-30;
	for (s=0; s<m_numSamples; s++) {
		curWeight = m_samples[s]->GetWeight()*(*p_cumulWeight);
		m_samples[s]->SetWeight(curWeight);
		weightSum += curWeight;
		p_cumulWeight++;
	}
	
	weightSumInv = 1.f/weightSum;
	for (s=0; s<m_numSamples; s++) {
		curWeight = m_samples[s]->GetWeight()*weightSumInv;
		m_samples[s]->SetWeight(curWeight);
	}
}

void PFGenericParticleFilter::ComputeMeanState()
{
	int i, s;
	float *p_sampleState, *p_meanState, likelihood;
	
	if (!m_samples) {
		throw new GeneralException ("PFGenericParticleFilter::ComputeMeanState : impossible to compute mean state with uninitialized samples.",__FILE__,__LINE__);
	}
	
	p_meanState = m_outMeanState->GetState();
	for (i=0; i<m_sampleStateSize; i++) {
		*p_meanState++ = 0.f;
	}
	
	// Perform a weighted sum to get the mean state
	for (s=0; s<m_numSamples; s++) {
		likelihood = m_samples[s]->GetWeight();
		p_sampleState = m_samples[s]->GetState();
		p_meanState = m_outMeanState->GetState();
		
		for (i=0; i<m_sampleStateSize; i++) {
			*p_meanState += (*p_sampleState++)*likelihood;
			p_meanState++;
		}
	}
}

void PFGenericParticleFilter::Resample()
{
	int s;
	float effectiveN = 0.f, curWeight, sizeInv, sumWeights;
	
	if (!m_samples) {
		throw new GeneralException ("PFGenericParticleFilter::Resample : impossible to resample with uninitialized samples.",__FILE__,__LINE__);
	}
	
	for (s=0; s<m_numSamples; s++) {
		curWeight = m_samples[s]->GetWeight();
		effectiveN += curWeight * curWeight;
	}
	
	effectiveN = 1.f/(effectiveN*m_numSamples);
	
	if (effectiveN > 0.6f) {
		// Current distribution modelled by the particles set has a sufficient density
		return;
	}
	
	CopyTmpSamples();
	
	m_cumulWeight[0] = m_tmpSamples[0]->GetWeight();
	for (s=1; s<m_numSamples; s++) {
		m_cumulWeight[s] = m_cumulWeight[s-1]+m_samples[s]->GetWeight();
	}
	
	sumWeights = 0.f;
	for (s=0; s<m_numSamples; s++) {
		float r = PFUTIL_ran();
		int j=PFUTIL_find_range(r, m_cumulWeight, m_numSamples);
		*(m_samples[s]) = *(m_tmpSamples[j]);
		m_samples[s]->SetWeight(1.f);
		sumWeights += m_samples[s]->GetWeight();
	}
	
	sumWeights = 1.f/sumWeights;
	for (s=0; s<m_numSamples; s++) {
		curWeight = m_samples[s]->GetWeight();
		m_samples[s]->SetWeight(curWeight*sumWeights);
	}
}

RCPtr<PFGenericParticle> PFGenericParticleFilter::GetCurSample()
{
	return m_curSample;
}
	
RCPtr<PFGenericParticle> PFGenericParticleFilter::GetFirstSample()
{
	return m_samples[0];
}
	
RCPtr<PFGenericParticle> PFGenericParticleFilter::GetSampleByIdx(int i_idx)
{
	if (i_idx < 0 || i_idx >= m_numSamples) {
		throw new GeneralException ("PFGenericParticleFilter::GetSampleByIdx : invalid sample index.",__FILE__,__LINE__);
	}
	
	return m_samples[i_idx];
}
	
RCPtr<PFGenericParticle> PFGenericParticleFilter::GetMeanState()
{
	return m_outMeanState;
}

void PFGenericParticleFilter::SetRefMeanSample(PFGenericParticle *i_curSample)
{
	if (m_refMeanState.get()) {
		m_refMeanState.detach();
	}
	
	m_refMeanState = RCPtr<PFGenericParticle>(i_curSample);
}

void PFGenericParticleFilter::InitLikelihoodsSum()
{
	m_likelihoodsSum = 0.f;
}
	
void PFGenericParticleFilter::IncLikelihoodsSum(float i_likelihood)
{
	m_likelihoodsSum += i_likelihood;
}


//
// Private methods
//

void PFGenericParticleFilter::CopyTmpSamples()
{
	int s;
	
	for (s=0; s<m_numSamples; s++) {
		*(m_tmpSamples[s]) = *(m_samples[s]);
	}
}

}

