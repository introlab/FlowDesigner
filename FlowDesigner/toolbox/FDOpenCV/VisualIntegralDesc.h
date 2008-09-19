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

#ifndef _VISUALINTEGRALCOLORDESC_H_
#define _VISUALINTEGRALCOLORDESC_H_

#include <math.h>
#include "VisualFeatureDesc.h"

namespace RobotFlow {

typedef enum
{
	e_VISUALINTDESC_EuclideanDist = 0,
	// TODO: maybe add other similarity methods
	e_VISUALINTDESC_UnknownSimilarity
} e_VISUALINTDESC_similarityType;

//
// Descriptor for integral image features
// A typical integral image feature consists of a mean value
// for a given region of interest in each channel of the image
// 
template <class FeatType>
class VisualIntegralDesc : public VisualFeatureDesc<FeatType>
{
public:
	VisualIntegralDesc()
	: VisualFeatureDesc<FeatType>(e_VISUALDESCRIPTOR_integral),
	m_simType(e_VISUALINTDESC_UnknownSimilarity), m_numClrChannels(0), 
	m_numIntValues(0), m_featSize(0), m_valid(false), m_useRectDiff(false),
	m_useBoundary(false), m_intFeatures(NULL)
	{ 
		SetSimilarityFct();
	}
	
	VisualIntegralDesc(e_VISUALINTDESC_similarityType i_simType,
		unsigned int i_numClrChannels, unsigned int i_numIntValues,
		FeatType i_maxValue, bool i_useRectDiff, bool i_useBoundary) 
	: VisualFeatureDesc<FeatType>(e_VISUALDESCRIPTOR_integral),
	m_simType(i_simType), m_numClrChannels(i_numClrChannels),
	m_numIntValues(i_numIntValues), m_maxValue(i_maxValue),
	m_valid(true), m_useRectDiff(i_useRectDiff), 
	m_useBoundary(i_useBoundary), m_intFeatures(NULL)
	{ 
		m_featSize = m_numClrChannels*m_numIntValues;
		
		if (m_useRectDiff) {
			m_maxDiffValue = m_maxValue;
			m_maxValue = (FeatType)((double)(m_maxDiffValue)*0.5);
			m_numMeanValues = (unsigned int)(sqrt((float)(m_numIntValues)));
			m_numIntValues -= m_numMeanValues;
		}
		
		if (m_useBoundary) {
			m_featSize += 4;
		}
		
		m_intFeatures = new FeatType[m_featSize];
		
		SetSimilarityFct();
	}
	
	VisualIntegralDesc(const VisualIntegralDesc<FeatType> &i_ref)
	{
		try {
			this->SetType(i_ref.GetType());
			m_simType = i_ref.m_simType;
			m_numClrChannels = i_ref.m_numClrChannels;
			m_numIntValues = i_ref.m_numIntValues;
			m_featSize = i_ref.m_featSize;
			m_maxValue = i_ref.m_maxValue;
			m_valid = i_ref.m_valid;
			m_useRectDiff = i_ref.m_useRectDiff;
			m_useBoundary = i_ref.m_useBoundary;
			m_maxDiffValue = i_ref.m_maxDiffValue;
			m_numMeanValues = i_ref.m_numMeanValues;
			
			m_intFeatures = new FeatType[m_featSize];
			
			for (int i=0; i<m_featSize; i++) {
				m_intFeatures[i] = i_ref.m_intFeatures[i];
			}
			
			SetSimilarityFct();
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualIntegralDesc::VisualIntegralDesc:",__FILE__,__LINE__));
		}
	}

	~VisualIntegralDesc()
	{
		delete [] m_intFeatures;
	}

	VisualIntegralDesc<FeatType> & operator =(const VisualIntegralDesc<FeatType> &i_ref)
	{
		// Avoid self assignment
		if (&i_ref == this) {
			return *this;
		}
		
		this->SetType(i_ref.GetType());
		m_simType = i_ref.m_simType;
		m_numClrChannels = i_ref.m_numClrChannels;
		m_numIntValues = i_ref.m_numIntValues;
		m_featSize = i_ref.m_featSize;
		m_maxValue = i_ref.m_maxValue;
		m_valid = i_ref.m_valid;
		m_useRectDiff = i_ref.m_useRectDiff;
		m_useBoundary = i_ref.m_useBoundary;
		m_maxDiffValue = i_ref.m_maxDiffValue;
		m_numMeanValues = i_ref.m_numMeanValues;
		
		delete [] m_intFeatures;
		m_intFeatures = new FeatType[m_featSize];
		
		for (int i=0; i<m_featSize; i++) {
			m_intFeatures[i] = i_ref.m_intFeatures[i];
		}
		
		SetSimilarityFct();
		
		return *this;
	}
	
	VisualIntegralDesc<FeatType>* clone()  const
	{
		return new VisualIntegralDesc<FeatType>(*this);
	}

	// Default routine to print a VisualIntegralDesc object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in VisualIntegralDesc::printOn: cannot use base class routine.",__FILE__,__LINE__);
	}

	// Default routine to read a VisualIntegralDesc object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in VisualIntegralDesc::readFrom: cannot use base class routine.",__FILE__,__LINE__);
	}

	double Similarity(const FeatType *i_candidate, unsigned int i_size) const
	{
		try {
			if (!i_candidate) {
				throw new FD::GeneralException("VisualIntegralDesc::Similarity: invalid (NULL) candidate features.",__FILE__,__LINE__);
			}
			
			if (i_size != m_featSize) {
				throw new FD::GeneralException("VisualIntegralDesc::Similarity: candidate features size differs from current features descriptor size.",__FILE__,__LINE__);
			}
			
			if (!m_similarityFct) {
				throw new FD::GeneralException("VisualIntegralDesc::Similarity: invalid or unknown similarity type.",__FILE__,__LINE__);
			}
			
			if (!m_valid) {
				// Invalid descriptor, output maximal distance
				return 0.0;
			}
			
			// Use appropriate function
			return (this->*m_similarityFct)(i_candidate);
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualIntegralDesc::Similarity:",__FILE__,__LINE__));
		}
	}
	
	void Adapt(const FeatType *i_candidate, unsigned int i_size, double i_rate)
	{
		try {
			if (!i_candidate) {
				throw new FD::GeneralException("VisualIntegralDesc::Adapt: invalid (NULL) candidate features.",__FILE__,__LINE__);
			}
			
			if (i_size != m_featSize) {
				throw new FD::GeneralException("VisualIntegralDesc::Adapt: candidate features size differs from current features descriptor size.",__FILE__,__LINE__);
			}
			
			if (i_rate < 0.0 || i_rate > 1.0) {
				throw new FD::GeneralException ("VisualHistogramDesc::Adapt : adaptation rate must be in the interval [0.0,1.0]",__FILE__,__LINE__);
			}
			
			if (i_rate == 0.0) {
				// Nothing to do
				return;
			}
			
			if (i_rate == 1.0) {
				SetFeatures(i_candidate, i_size);
				return;
			}
			
			int i;
			const FeatType *p_adaptFeat = i_candidate;
			double compRate = 1.0 - i_rate;
			
			for (i=0; i<m_featSize; i++) {
				m_intFeatures[i] = (FeatType)(compRate*(double)(m_intFeatures[i]) + i_rate*(double)(p_adaptFeat[i]));
			}
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualIntegralDesc::Adapt:",__FILE__,__LINE__));
		}
	}
	
	unsigned int GetSize() const
	{
		return m_featSize;
	}

	FeatType *GetFeatures()
	{
		return m_intFeatures;
	}
	
	const FeatType *GetCstFeatures() const
	{
		return (const FeatType *)m_intFeatures;
	}
	
	bool GetValidity() const
	{
		return m_valid;
	}
	
	void SetSize(unsigned int i_size)
	{
		throw new FD::GeneralException("Exception in VisualIntegralDesc::SetSize: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	void SetFeatures(const FeatType *i_ref, unsigned int i_size)
	{
		try {
			if (!i_ref) {
				throw new FD::GeneralException("VisualIntegralDesc::SetFeatures: invalid (NULL) input features.",__FILE__,__LINE__);
			}
			
			if (i_size != m_featSize) {
				throw new FD::GeneralException("VisualIntegralDesc::SetFeatures: candidate features size differs from current features descriptor size.",__FILE__,__LINE__);
			}
			
			int i;
			const FeatType *p_inFeat = i_ref;
			
			for (i=0; i<m_featSize; i++) {
				m_intFeatures[i] = *p_inFeat++;
			}
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualIntegralDesc::SetFeatures:",__FILE__,__LINE__));
		}
	}
	
	void SetValidity(bool i_flag) {
		m_valid = i_flag;
	}
	
private:
	void SetSimilarityFct()
	{
		if (m_simType == e_VISUALINTDESC_EuclideanDist) {
			if (m_useRectDiff) {
				if (m_useBoundary) {
					m_similarityFct = &VisualIntegralDesc::EuclideanDistWDiffWBound;
				}
				else {
					m_similarityFct = &VisualIntegralDesc::EuclideanDistWDiff;
				}
			}
			else {
				if (m_useBoundary) {
					m_similarityFct = &VisualIntegralDesc::EuclideanDistWBound;
				}
				else {
					m_similarityFct = &VisualIntegralDesc::EuclideanDist;
				}
			}
		}
		else {
			m_similarityFct = NULL;
		}
	}
	
	double EuclideanDist(const FeatType *i_candidate) const
	{
		FeatType diff;
		double dist = 0.0;
		const FeatType *p_curFeat = (const FeatType *)m_intFeatures;
		const FeatType *p_candFeat = i_candidate;
	
		for (int i=0; i<m_numIntValues; i++) {
			double clrDist = 0.0;
			for (int c=0; c<m_numClrChannels; c++) {
				diff = ((*p_curFeat) - (*p_candFeat))/m_maxValue;
				clrDist += (double)(diff*diff);
				p_curFeat++;
				p_candFeat++;
			}
			dist += sqrt(clrDist);
		}
		
		dist /= m_numIntValues;
		
		if (dist > 1.0) {
			return 0.0;
		}
	
		return 1.0-dist;
	}
	
	double EuclideanDistWDiff(const FeatType *i_candidate) const
	{
		int i, c;
		FeatType diff;
		double dist, dist1 = 0.0, dist2 = 0.0;
		const FeatType *p_curFeat = (const FeatType *)m_intFeatures;
		const FeatType *p_candFeat = i_candidate;
	
		for (i=0; i<m_numMeanValues; i++) {
			double clrDist = 0.0;
			for (c=0; c<m_numClrChannels; c++) {
				diff = ((*p_curFeat) - (*p_candFeat))/m_maxValue;
				clrDist += (double)(diff*diff);
				p_curFeat++;
				p_candFeat++;
			}
			dist1 += sqrt(clrDist);
		}

		dist1 /= m_numMeanValues;

		if (dist1 > 1.0) {
			return 0.0;
		}
		
		for (i=0; i<m_numIntValues; i++) {
			double clrDist = 0.0;
			for (c=0; c<m_numClrChannels; c++) {
				diff = ((*p_curFeat) - (*p_candFeat))/m_maxDiffValue;
				clrDist += (double)(diff*diff);
				
				p_curFeat++;
				p_candFeat++;
			}
			dist2 += sqrt(clrDist);
		}
		
		dist2 /= m_numIntValues;
		
		if (dist2 > 1.0) {
			return 0.0;
		}
		
		dist = (dist1+dist2)*0.5;
		
		if (dist > 1.0) {
			return 0.0;
		}
	
		return 1.0-dist;
	}
	
	double EuclideanDistWBound(const FeatType *i_candidate) const
	{
		int i;
		FeatType diff;
		double dist = 0.0;
		const FeatType *p_curFeat = (const FeatType *)m_intFeatures;
		const FeatType *p_candFeat = i_candidate;
	
		for (i=0; i<m_numIntValues; i++) {
			double clrDist = 0.0;
			for (int c=0; c<m_numClrChannels; c++) {
				diff = ((*p_curFeat) - (*p_candFeat))/m_maxValue;
				clrDist += (double)(diff*diff);
				p_curFeat++;
				p_candFeat++;
			}
			dist += sqrt(clrDist);
		}
		
		dist /= m_numIntValues;
		
		if (dist > 1.0) {
			return 0.0;
		}
		
		// Check for an obvious boundary in candidate
		int numBoundary = 0;
		int numUnknown = 0;
		for (i=0; i<4; i++) {
			if (*p_candFeat == 1) {
				numUnknown++;
			}
			else if (*p_candFeat == 2) {
				numBoundary++;
			}
		}
		
		if (numUnknown < 4) {
			int numValid = 4-numUnknown;
			dist += (double)(numValid-numBoundary)/(double)(numValid);
			
			if (dist > 1.0) {
				return 0.0;
			}
		}
		
		return 1.0-dist;
	}
	
	double EuclideanDistWDiffWBound(const FeatType *i_candidate) const
	{
		int i, c;
		FeatType diff;
		double dist, dist1 = 0.0, dist2 = 0.0;
		const FeatType *p_curFeat = (const FeatType *)m_intFeatures;
		const FeatType *p_candFeat = i_candidate;
	
		for (i=0; i<m_numMeanValues; i++) {
			double clrDist = 0.0;
			for (c=0; c<m_numClrChannels; c++) {
				diff = ((*p_curFeat) - (*p_candFeat))/m_maxValue;
				clrDist += (double)(diff*diff);
				p_curFeat++;
				p_candFeat++;
			}
			dist1 += sqrt(clrDist);
		}

		dist1 /= m_numMeanValues;
		
		if (dist1 > 1.0) {
			return 0.0;
		}
		
		for (i=0; i<m_numIntValues; i++) {
			double clrDist = 0.0;
			for (c=0; c<m_numClrChannels; c++) {
				diff = ((*p_curFeat) - (*p_candFeat))/m_maxDiffValue;
				clrDist += (double)(diff*diff);
				p_curFeat++;
				p_candFeat++;
			}
			dist2 += sqrt(clrDist);
		}
		
		dist2 /= m_numIntValues;
		
		if (dist2 > 1.0) {
			return 0.0;
		}
		
		dist = (dist1+dist2)*0.5;
		
		if (dist > 1.0) {
			return 0.0;
		}
	
		// Check for an obvious boundary in candidate
		int numBoundary = 0;
		int numUnknown = 0;
		for (i=0; i<4; i++) {
			if (*p_candFeat == 1) {
				numUnknown++;
			}
			else if (*p_candFeat == 2) {
				numBoundary++;
			}
		}
		
		if (numUnknown < 4) {
			int numValid = 4-numUnknown;
			dist += (double)(numValid-numBoundary)/(double)(numValid);
			
			if (dist > 1.0) {
				return 0.0;
			}
		}
		
		return 1.0-dist;
	}

private:
	// Similarity/Distance type to use
	e_VISUALINTDESC_similarityType m_simType;
	
	// Number of color channels of the color space used
	unsigned int m_numClrChannels;
	
	// Number of integral rectangle values/features
	unsigned int m_numIntValues;
	
	// The size of the feature descriptor
	unsigned int m_featSize;
	
	// Maximum channel value 
	FeatType m_maxValue;
	
	unsigned int m_numMeanValues;
	FeatType m_maxDiffValue;
	
	bool m_useRectDiff;
	
	bool m_useBoundary;
	
	// Validity flag in cases where ROI was invalid for features extraction
	bool m_valid;
	
	// Integral features
	FeatType *m_intFeatures;
	
	// Function pointer to the appropriate private similarity routine
	double (VisualIntegralDesc::*m_similarityFct)(const FeatType *) const;
};

}

#endif
