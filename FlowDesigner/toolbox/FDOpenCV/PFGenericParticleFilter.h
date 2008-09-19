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

#ifndef _PFGENERICPARTICLEFILTER_H_
#define _PFGENERICPARTICLEFILTER_H_

#include "PFParticleFilter.h"
#include "PFGenericParticle.h"

namespace RobotFlow {
//
// Generic implementation of a particle filter
// For more information, refer to the following publication
// M. S. Arulampalam, S. Maskell, N. Gordon, T. Clapp.
// "A Tutorial on Particle Filters for On-line Nonlinear/Non-Gaussian 
// Bayesian Tracking"  (2001) 
//
class PFGenericParticleFilter : public PFParticleFilter
{
public:
	PFGenericParticleFilter();
	
	PFGenericParticleFilter(unsigned int i_numSamples, 
		unsigned int i_sampleStateSize, const FD::Vector<float> *i_initVariance);
	
	PFGenericParticleFilter(std::string nodeName, FD::ParameterSet params);

	virtual ~PFGenericParticleFilter();

	// Default routine to print a PFGenericParticleFilter object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in PFGenericParticleFilter::printOn: method not yet implemented.",__FILE__,__LINE__);
	}

	// Default routine to read a PFGenericParticleFilter object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in PFGenericParticleFilter::printOn: method not yet implemented.",__FILE__,__LINE__);
	}
	
	virtual void request(int output_id, const FD::ParameterSet &req);
	
	void calculate(int output_id, int count, FD::Buffer &out);
	
	void Initialize(const FD::Vector<float> *i_variance);
	
	void InitSamples();
	
	void Update();
	
	void ComputeMeanState();
	
	void Resample();
	
	FD::RCPtr<PFGenericParticle> GetCurSample();
	
	FD::RCPtr<PFGenericParticle> GetFirstSample();
	
	FD::RCPtr<PFGenericParticle> GetSampleByIdx(int i_idx);
	
	FD::RCPtr<PFGenericParticle> GetMeanState();
	
	void SetRefMeanSample(PFGenericParticle *i_curSample);
	
	void InitLikelihoodsSum();
	
	void IncLikelihoodsSum(float i_likelihood);

private:
	void CopyTmpSamples();

private:
	// Input IDs (for BufferedNode)
	int m_initVarianceInID;
	int m_refMeanStateInID;
	int m_predictInID;
	int m_likelihoodInID;
	
	// Output IDs (for BufferedNode)
	int m_finishedOutID;
	int m_particleOutID;
	int m_meanStateOutID;
	
	bool m_init;
	bool m_initPF;
	bool m_initSamples;
	bool m_finished;
	
	int m_curSampleIdx;
	unsigned int m_numSamples;
	unsigned int m_sampleStateSize;
	FD::RCPtr<PFGenericParticle> *m_samples;
	FD::RCPtr<PFGenericParticle> *m_tmpSamples;
	float m_likelihoodsSum;
	float *m_cumulWeight;
	FD::Vector<float> *m_initVariance;
	
	FD::RCPtr<PFGenericParticle> m_curSample;
	FD::RCPtr<PFGenericParticle> m_refMeanState;
	FD::RCPtr<PFGenericParticle> m_outMeanState;
};

}

#endif
