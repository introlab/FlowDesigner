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

#ifndef _VISUALTARGET_H_
#define _VISUALTARGET_H_

#include "BufferedNode.h"
#include <iostream>
#include "Image.h"
#include "cv.h"
#include "Vector.h"
#include "VisualFeatureDesc.h"
#include "VisualROI.h"

namespace RobotFlow {

template <class FeatBaseType>
class VisualTarget : public FD::Object
{
public:
	VisualTarget()
	: m_valid(false), m_id(0), m_activeAge(0), m_passiveAge(0), m_curDescIdx(0),
	m_refROI(NULL), m_numDesc(-1), m_targetDesc(NULL),
	m_cueWeights(NULL), m_tmpCueProb(NULL)
	{
		
	}
	
	VisualTarget(int i_id, VisualROI *i_roi,
		FD::Vector<VisualFeatureDesc<FeatBaseType> *> *i_targetDesc)
	: m_valid(true), m_id(i_id), m_activeAge(0), m_passiveAge(0), m_curDescIdx(0),
	m_targetDesc(NULL), m_cueWeights(NULL), m_tmpCueProb(NULL)
	{
		m_refROI = FD::RCPtr<VisualROI>(new VisualROI(*i_roi));
		m_numDesc = i_targetDesc->size();
		m_targetDesc = new FD::Vector<VisualFeatureDesc<FeatBaseType> *>(m_numDesc);
		
		for (int i=0; i<m_numDesc; i++) {
			(*m_targetDesc)[i] = new VisualFeatureDesc<FeatBaseType>(*((*i_targetDesc)[i]));
		}
		
		m_cueWeights = new double[m_numDesc];
		m_tmpCueProb = new double[m_numDesc];
	}
	
	VisualTarget(const VisualTarget<FeatBaseType> & i_ref)
	:m_id(-1), m_activeAge(-1), m_passiveAge(-1), m_curDescIdx(0),
	m_numDesc(-1), m_targetDesc(NULL), m_cueWeights(NULL),
	m_tmpCueProb(NULL)
	{
		try {
			m_valid = i_ref.m_valid;
			m_id = i_ref.m_id;
			m_activeAge = i_ref.m_activeAge;
			m_passiveAge = i_ref.m_passiveAge;
			m_curDescIdx = i_ref.m_curDescIdx;
			if (!i_ref.m_refROI.isNil()) {
				if (!this->m_refROI.isNil()) {
					*(this->m_refROI) = *(i_ref.m_refROI);
				}
				else {
					this->m_refROI = FD::RCPtr<VisualROI>(new VisualROI(*(i_ref.m_refROI)));
				}
			}
			else {
				std::cout << "VisualTarget::VisualTarget: reference to copy has NULL ROI" << std::endl;
			}
			if (i_ref.m_targetDesc) {
				SetDescriptorsVec(i_ref.m_targetDesc);
			}
			else {
				this->m_targetDesc = NULL;
				this->m_numDesc = 0;
				this->m_cueWeights = NULL;
				this->m_tmpCueProb = NULL;
			}
			for (int i=0; i<m_numDesc; i++) {
				m_cueWeights[i] = i_ref.m_cueWeights[i];
				m_tmpCueProb[i] = i_ref.m_tmpCueProb[i];
			}
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualTarget::VisualTarget:",__FILE__,__LINE__));
		}
	}

	~VisualTarget()
	{
		for (int i=0; i<m_numDesc; i++) {
			delete (*m_targetDesc)[i];
		}
		delete m_targetDesc;
		delete [] m_cueWeights;
		delete [] m_tmpCueProb;
	}
	
	VisualTarget<FeatBaseType> & operator =(VisualTarget<FeatBaseType> &i_ref)
	{
		try {
			// Avoid self assignment
			if (&i_ref != this) {
				this->m_valid = i_ref.m_valid;
				this->m_id = i_ref.m_id;
				this->m_activeAge = i_ref.m_activeAge;
				this->m_passiveAge = i_ref.m_passiveAge;
				this->m_curDescIdx = i_ref.m_curDescIdx;
				if (!i_ref.m_refROI.isNil()) {
					if (!this->m_refROI.isNil()) {
						*(this->m_refROI) = *(i_ref.m_refROI);
					}
					else {
						this->m_refROI = FD::RCPtr<VisualROI>(new VisualROI(*(i_ref.m_refROI)));
					}
				}
				else {
					std::cout << "VisualTarget::VisualTarget: reference to copy has NULL ROI" << std::endl;
				}
				if (i_ref.m_targetDesc) {
					SetDescriptorsVec(i_ref.m_targetDesc);
				}
				else {
					this->m_targetDesc = NULL;
					this->m_cueWeights = NULL;
					this->m_tmpCueProb = NULL;
					this->m_numDesc = 0;
				}
				for (int i=0; i<m_numDesc; i++) {
					m_cueWeights[i] = i_ref.m_cueWeights[i];
					m_tmpCueProb[i] = i_ref.m_tmpCueProb[i];
				}
			}
			
			return *this;
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualTarget::operator=:",__FILE__,__LINE__));
		}
	}

	// Default routine to print a VisualTarget object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in VisualTarget::printOn: method not yet implemented.",__FILE__,__LINE__);
	}

	// Default routine to read a VisualTarget object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in VisualTarget::readFrom: method not yet implemented.",__FILE__,__LINE__);
	}
	
	void Adapt(FD::Vector<VisualFeatureDesc<FeatBaseType> *> *i_desc, double i_rate)
	{
		if (m_numDesc != i_desc->size()) {
			throw new FD::GeneralException("Exception in VisualTarget::Adapt: input descriptor vector size differs from current object's vector.",__FILE__,__LINE__);
		}
		
		for (int i=0; i<m_numDesc; i++) {
			if ((*m_targetDesc)[i]->GetType() != (*i_desc)[i]->GetType()) {
				throw new FD::GeneralException("Exception in VisualTarget::Adapt: features descriptor must have the same type.",__FILE__,__LINE__);
			}
			
			(*m_targetDesc)[i]->Adapt((*i_desc)[i]->GetCstFeatures(), (*i_desc)[i]->GetSize(), i_rate);
		}
	}

	void Adapt(FD::Vector<VisualFeatureDesc<FeatBaseType> *> *i_desc, double *i_rate)
	{
		if (m_numDesc != i_desc->size()) {
			throw new FD::GeneralException("Exception in VisualTarget::Adapt: input descriptor vector size differs from current object's vector.",__FILE__,__LINE__);
		}
		
		for (int i=0; i<m_numDesc; i++) {
			if ((*m_targetDesc)[i]->GetType() != (*i_desc)[i]->GetType()) {
				throw new FD::GeneralException("Exception in VisualTarget::Adapt: features descriptor must have the same type.",__FILE__,__LINE__);
			}
			
			(*m_targetDesc)[i]->Adapt((*i_desc)[i]->GetCstFeatures(), (*i_desc)[i]->GetSize(), i_rate[i]);
		}
	}
	
	double Similarity(FD::Vector<VisualFeatureDesc<FeatBaseType> *> *i_desc)
	{
		if (m_numDesc != i_desc->size()) {
			throw new FD::GeneralException("Exception in VisualTarget::Similarity: input descriptor vector size differs from current object's vector.",__FILE__,__LINE__);
		}
		
		double sim = 0.0;
		
		for (int i=0; i<m_numDesc; i++) {
			if ((*m_targetDesc)[i]->GetType() != (*i_desc)[i]->GetType()) {
				throw new FD::GeneralException("Exception in VisualTarget::Similarity: features descriptor must have the same type.",__FILE__,__LINE__);
			}
			
			sim += m_cueWeights[i]*(*m_targetDesc)[i]->Similarity((*i_desc)[i]->GetCstFeatures(), (*i_desc)[i]->GetSize());
		}
		
		return sim;
	}
	
	double SimilarityWCueAdapt(FD::Vector<VisualFeatureDesc<FeatBaseType> *> *i_desc, double i_rate)
	{
		if (m_numDesc != i_desc->size()) {
			throw new FD::GeneralException("Exception in VisualTarget::SimilarityWCueAdapt: input descriptor vector size differs from current object's vector.",__FILE__,__LINE__);
		}
		
		int i;
		double sim = 0.0, cueSim, sumCueSim = 0.0;
		
		for (i=0; i<m_numDesc; i++) {
			if ((*m_targetDesc)[i]->GetType() != (*i_desc)[i]->GetType()) {
				throw new FD::GeneralException("Exception in VisualTarget::SimilarityWCueAdapt: features descriptor must have the same type.",__FILE__,__LINE__);
			}
			
			m_tmpCueProb[i] = (*m_targetDesc)[i]->Similarity((*i_desc)[i]->GetCstFeatures(), (*i_desc)[i]->GetSize());
			sumCueSim += m_tmpCueProb[i];
			sim += m_cueWeights[i]*m_tmpCueProb[i];
			std::cout << "Cue Weights [" << i << "]=" << m_cueWeights[i] << " Current Cue Prob=" << m_tmpCueProb[i] << std::endl;
		}
		
		if (sumCueSim == 0.0) {
			std::cerr << "WARNING: VisualTarget::SimilarityWCueAdapt: cannot adapt cue weights with a total probability of 0.0." << std::endl;
			return sim;
		}
		
		// Adapt cue weight
		double rateInv = 1.0 - i_rate;
		double sumCueSimInv = 1.0/sumCueSim;
		sumCueSim = 0.0;
		
		for (i=0; i<m_numDesc; i++) {
			m_cueWeights[i] = i_rate*m_tmpCueProb[i]*sumCueSimInv + rateInv*m_cueWeights[i];
			sumCueSim += m_cueWeights[i];
		}
		
		// Normalize weights
		sumCueSimInv = 1.0/sumCueSim;
		if (sumCueSimInv == 0.0) {
			throw new FD::GeneralException("Exception in VisualTarget::SimilarityWCueAdapt: cannot have a target with all cue weights are zero.",__FILE__,__LINE__);
		}
		for (i=0; i<m_numDesc; i++) {
			m_cueWeights[i] *= sumCueSimInv;
		}
		
		return sim;
	}
	
	void AgeTarget(bool i_matched) 
	{
		if (i_matched) {
			if (m_activeAge < 15) {
				m_activeAge++;
			}
			if (m_passiveAge > 0) {
				m_passiveAge--;
			} 
		}
		else {
			if (m_activeAge < 15) {
				m_passiveAge++;
			}
			if (m_activeAge > 0) {
				m_activeAge--;
			} 
		}
	}
	
	bool IsValid() const 
	{
		return m_valid;
	}
	
	int GetID() const
	{
		return m_id;
	}
	
	int GetActiveAge() const
	{
		return m_activeAge;
	}
	
	int GetPassiveAge() const
	{
		return m_passiveAge;
	}
	
	int GetCurrentAge() const
	{
		return m_activeAge - m_passiveAge;
	}
	
	VisualROI *GetROI()
	{
		return m_refROI.get();
	}
	
	const VisualROI *GetCstROI() const
	{
		return (const VisualROI *)(m_refROI.get());
	}
	
	FD::RCPtr<VisualROI> GetROIRCPtr()
	{
		return m_refROI;
	}
	
	int GetNumDescriptors() const
	{
		return m_numDesc;
	}
	
	int GetCurDescIdx() const
	{
		return m_curDescIdx;
	}
	
	FD::Vector<VisualFeatureDesc<FeatBaseType> *> *GetDescriptorsVec()
	{
		return m_targetDesc;
	}
	
	const FD::Vector<VisualFeatureDesc<FeatBaseType> *> *GetCstDescriptorsVec() const
	{
		return (const FD::Vector<VisualFeatureDesc<FeatBaseType> *> *)m_targetDesc;
	}
	
	VisualFeatureDesc<FeatBaseType> *GetDescriptor(int i_idx)
	{
		if (i_idx >= m_numDesc) {
			throw new FD::GeneralException("Exception in VisualTarget::GetDescriptor: descriptor index is greater than the actual number of descriptors in current vector.",__FILE__,__LINE__);
		}
		
		return (*m_targetDesc)[i_idx];
	}
	
	const VisualFeatureDesc<FeatBaseType> *GetCstDescriptor(int i_idx) const
	{
		if (i_idx >= m_numDesc) {
			throw new FD::GeneralException("Exception in VisualTarget::GetCstDescriptor: descriptor index is greater than the actual number of descriptors in current vector.",__FILE__,__LINE__);
		}
		
		return (const VisualFeatureDesc<FeatBaseType> *)((*m_targetDesc)[i_idx]);
	}
	
	double *GetCueWeights()
	{
		return m_cueWeights;
	}
	
	const double *GetCueWeights() const 
	{
		return (const double *)m_cueWeights;
	}
	
	double GetCueWeight(int i_idx) const
	{
		if (i_idx >= m_numDesc) {
			throw new FD::GeneralException("Exception in VisualTarget::GetCueWeight: index is greater than the actual number of descriptors in current vector.",__FILE__,__LINE__);
		}

		return m_cueWeights[i_idx];
	}
	
	void SetValidity(bool i_valid)
	{
		m_valid = i_valid;
	}
	
	void SetID(int i_id) 
	{
		m_id = i_id;
	}
	
	void InitAges()
	{
		m_activeAge = 0;
		m_passiveAge = 0;
	}
	
	void SetActiveAge(int i_age)
	{
		m_activeAge = i_age;
	}
	
	void SetPassiveAge(int i_age)
	{
		m_passiveAge = i_age;
	}
	
	void SetROI(VisualROI *i_roi)
	{
		if (!m_refROI.isNil()) {
			*m_refROI = *i_roi;
		}
		else {
			m_refROI = FD::RCPtr<VisualROI>(new VisualROI(*i_roi));
		}
	}
	
	// Requires to set descriptors afterwards
	void SetNumDescriptors(int i_numDesc)
	{
		if (m_targetDesc) {
			// First deallocate memory
			for (int i=0; i<m_numDesc; i++) {
				delete (*m_targetDesc)[i];
			}
		}
		
		if (m_numDesc != i_numDesc) {
			delete m_targetDesc;
			delete [] m_cueWeights;
			delete [] m_tmpCueProb;
			
			// Reset vector size
			m_numDesc = i_numDesc;
			m_targetDesc = new FD::Vector<VisualFeatureDesc<FeatBaseType> *>(m_numDesc);
			m_cueWeights = new double[m_numDesc];
			m_tmpCueProb = new double[m_numDesc];
		}
	}
	
	void SetCurDescIdx(int i_idx)
	{
		if (i_idx >= m_numDesc) {
			throw new FD::GeneralException("Exception in VisualTarget::SetCurDescIdx: descriptor index is greater than the actual number of descriptors in current vector.",__FILE__,__LINE__);
		}
		
		m_curDescIdx = i_idx;
	}
	
	void SetDescriptorsVec(FD::Vector<VisualFeatureDesc<FeatBaseType> *> *i_descVec)
	{
		// Reset the number of descriptors
		if (i_descVec->size() > 0 ) {
			this->SetNumDescriptors(i_descVec->size());
			for (int i=0; i<m_numDesc; i++) {
				(*m_targetDesc)[i] = (*i_descVec)[i]->clone();
			}
		}
		else {
			throw new FD::GeneralException("Exception in VisualTarget::SetDescriptorsVec: input feature vector must have at least one element.",__FILE__,__LINE__);
		}
	}
	
	void SetDescriptor(VisualFeatureDesc<FeatBaseType> *i_desc, int i_idx)
	{
		if (i_idx >= m_numDesc) {
			throw new FD::GeneralException("Exception in VisualTarget::SetDescriptor: descriptor index is greater than the actual number of descriptors in current vector.",__FILE__,__LINE__);
		}

		*((*m_targetDesc)[i_idx]) = *i_desc;
	}
	
	void InitCueWeights()
	{
		double initWeight = 1.0/(double)m_numDesc;
		
		for (int i=0; i<m_numDesc; i++) {
			m_cueWeights[i] = initWeight;
		}
	}
	
	void SetCueWeights(double *i_weight)
	{
		for (int i=0; i<m_numDesc; i++) {
			m_cueWeights[i] = i_weight[i];
		}
	}
	
	void SetCueWeight(double i_weight, int i_idx)
	{
		if (i_idx >= m_numDesc) {
			throw new FD::GeneralException("Exception in VisualTarget::SetCueWeight: index is greater than the actual number of descriptors in current vector.",__FILE__,__LINE__);
		}

		m_cueWeights[i_idx] = i_weight;
	}

private:
	bool m_valid;
	int m_id;
	int m_activeAge;
	int m_passiveAge;
	FD::RCPtr<VisualROI> m_refROI;
	int m_numDesc;
	int m_curDescIdx;
	FD::Vector<VisualFeatureDesc<FeatBaseType> *> *m_targetDesc;
	double *m_cueWeights;
	double *m_tmpCueProb;
};

}

#endif
