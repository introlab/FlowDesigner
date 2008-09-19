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

#ifndef _VISUALHISTOGRAMDESC_H_
#define _VISUALHISTOGRAMDESC_H_

#include "Object.h"
#include <iostream>
#include <math.h>
#include "VisualFeatureDesc.h"
#include "VisualROI.h"
#include "Vector.h"

namespace RobotFlow {

typedef enum
{
	e_VISUALHIST_BhattacharyyaCoeff = 0,
	e_VISUALHIST_BhattacharyyaDist,
	e_VISUALHIST_HistIntersection,
	e_VISUALHIST_EuclideanDist,
	// TODO: add other similarity methods
	e_VISUALHIST_unknownSimilarity
} e_VISUALHIST_similarityType;

//
// BinType will be used to save each bin value
//  and should only be basic c/c++ types
//  e.g. double, int, char, ...
//
// FeatType is the type used for the feature map
//  that will used to compute the histogram
//  e.g. unsigned char if the feature map is
//  an image
//
template <class BinType, class FeatType>
class VisualHistogramDesc : public VisualFeatureDesc<BinType>
{
public:
        static const double k_VISUALHIST_2DIVPI;

	VisualHistogramDesc()
	: VisualFeatureDesc<BinType>(e_VISUALDESCRIPTOR_histogram),
	m_simType(e_VISUALHIST_unknownSimilarity),
	m_valid(false),
	m_normFlag(false),
	m_numDimensions(0),
	m_numBins(NULL),
	m_binsWidth(NULL),
	m_bins(NULL)
	{
	}
	
	VisualHistogramDesc(e_VISUALHIST_similarityType i_simType, 
		bool i_normFlag, unsigned int i_numDimensions, 
		const FD::Vector<int> *i_numBins)
	: VisualFeatureDesc<BinType>(e_VISUALDESCRIPTOR_histogram),
	m_simType(i_simType),
	m_valid(true),
	m_normFlag(i_normFlag),
	m_numDimensions(i_numDimensions),
	m_numBins(NULL),
	m_binsWidth(NULL),
	m_bins(NULL)
	{
		unsigned int totalBins = 1;
		m_numBins = new int[m_numDimensions];

		for(int d=0; d<m_numDimensions; d++) {
			m_numBins[d] = (*i_numBins)[d];
			totalBins *= m_numBins[d];
		}

		m_totalBins = totalBins;
		m_bins = new BinType[m_totalBins];
		
		m_binsWidth = new FeatType[m_numDimensions];
		m_lastMaxVal = (FeatType)0;
		
		SetSimilarityFct();
		
		SetAdaptFct();
	}
	
	VisualHistogramDesc(const VisualHistogramDesc<BinType,FeatType> &i_ref)
	: m_numBins(NULL),
	m_binsWidth(NULL),
	m_bins(NULL)
	{
		this->SetType(i_ref.GetType());
		m_valid = i_ref.m_valid;
		m_simType = i_ref.m_simType;
		m_normFlag = i_ref.m_normFlag;
		
		SetSimilarityFct();
		
		SetAdaptFct();
		
		m_numDimensions = i_ref.m_numDimensions;
		m_numBins = new int[m_numDimensions];
		m_totalBins = i_ref.m_totalBins;
		m_bins = new BinType[m_totalBins];
		m_binsWidth = new FeatType[m_numDimensions];
		
		m_lastMaxVal = i_ref.m_lastMaxVal;
		
		for (int i=0; i<m_numDimensions; ++i) {
			m_binsWidth[i] = i_ref.m_binsWidth[i];
		}
	
		for(int d=0; d<m_numDimensions; d++) {
			m_numBins[d] = i_ref.m_numBins[d];
		}
	
		for (int b=0; b<m_totalBins; b++) {
			m_bins[b] = i_ref.m_bins[b];
		}
	}

	~VisualHistogramDesc()
	{
		delete [] m_numBins;
		delete [] m_binsWidth;
		delete [] m_bins;
	}

	VisualHistogramDesc<BinType,FeatType> & operator =(const VisualHistogramDesc<BinType,FeatType> &i_ref)
	{
		// Avoid self assignment
		if (&i_ref == this) {
			return *this;
		}
		
		this->SetType(i_ref.GetType());
		m_simType = i_ref.m_simType;
		m_valid = i_ref.m_valid;
		m_normFlag = i_ref.m_normFlag;
		
		SetSimilarityFct();
		
		SetAdaptFct();
	
		// Reallocate memory only if required
		if (m_numDimensions != i_ref.m_numDimensions) {
			delete [] m_numBins;
			delete [] m_binsWidth;
			
			m_numDimensions = i_ref.m_numDimensions;
			m_numBins = new int[m_numDimensions];
			m_binsWidth = new FeatType[m_numDimensions];
		}
		
		if (m_totalBins != i_ref.m_totalBins) {
			delete [] m_bins;
			m_totalBins = i_ref.m_totalBins;
			m_bins = new BinType[m_totalBins];
		}
		
		m_lastMaxVal = i_ref.m_lastMaxVal;
		
		for (int i=0; i<m_numDimensions; ++i) {
			m_binsWidth[i] = i_ref.m_binsWidth[i];
		}
	
		for(int d=0; d<m_numDimensions; d++) {
			m_numBins[d] = i_ref.m_numBins[d];
		}
	
		for (int b=0; b<m_totalBins; b++) {
			m_bins[b] = i_ref.m_bins[b];
		}
	
		return *this;
	}
	
	VisualHistogramDesc<BinType,FeatType>* clone()  const
	{
		return new VisualHistogramDesc<BinType,FeatType>(*this);
	}

	// Default routine to print a VisualHistogramDesc object to an output stream
	void printOn(std::ostream &out) const
	{
		int b, d;
		out << "<VisualHistogramDesc " << std::endl;
		out << "<NumDimensions " << m_numDimensions << " >" << std::endl;
		out << "<TotalBins " << m_totalBins << " >" << std::endl;
		out << "<SimilarityType " << (int)m_simType << " >" << std::endl;
		out << "<Validity " << (int)m_valid << " >" << std::endl;
		out << "<NormFlag " << (int)m_normFlag << " >" << std::endl;
		out << "<LastMaxVal " << m_lastMaxVal << " >" << std::endl;

		out << "<NumBins " << std::endl;
		for (d=0; d<m_numDimensions; d++)  {
			out << m_numBins[d] << " ";
		}
		out << " >" << std::endl;
		
		out << "<BinsWidth " << std::endl;
		for (d=0; d<m_numDimensions; d++)  {
			out << m_binsWidth[d] << " ";
		}
		out << " >" << std::endl;
	
		out << "<Bins " << std::endl;
		for (b=0; b<m_totalBins; ++b)  {
			out << m_bins[b] << " ";
		}
		out << " >" << std::endl;
	
		out << " >" << std::endl;
	}

	// Default routine to read a VisualHistogramDesc object from an input stream
	void readFrom(std::istream &in)
	{
		std::string tag;

		while (1) {
			char ch;
			in >> ch;
	
			if (ch == '>') {
				break;
			}
			else if (ch != '<') {
				throw new FD::GeneralException ("VisualHistogramDesc::readFrom : Parse error: '<' expected",__FILE__,__LINE__);
			}
	
			in >> tag;
	
			if (tag == "VisualHistogramDesc") {
				continue;
			}
			else if (tag == "NumDimensions") {
				in >> m_numDimensions;
	
				if (m_numDimensions < 1) {
					throw new FD::GeneralException ("VisualHistogramDesc::readFrom : invalid number of dimensions",__FILE__,__LINE__);
				}
			}
			else if (tag == "TotalBins") {
				in >> m_totalBins;
	
				if (m_totalBins < 1) {
					throw new FD::GeneralException ("VisualHistogramDesc::readFrom : invalid total number of bins",__FILE__,__LINE__);
				}
			}
			else if (tag == "SimilarityType") {
				int val;
				in >> val;
				m_simType = e_VISUALHIST_similarityType(val);
	
				SetSimilarityFct();
			}
			else if (tag == "Validity") {
				int val;
				in >> val;
				m_valid = bool(val);
			}
			else if (tag == "NormFlag") {
				int val;
				in >> val;
				m_normFlag = bool(val);
				
				SetAdaptFct();
			}
			else if (tag == "LastMaxVal") {
				in >> m_lastMaxVal;
			}
			else if (tag == "NumBins") {
				// Allocate memory
				delete [] m_numBins;
				m_numBins = new int[m_numDimensions];
	
				for (int d=0; d<m_numDimensions; d++) {
					in >> m_numBins[d];
				}
			}
			else if (tag == "BinsWidth") {
				// Allocate memory
				delete [] m_binsWidth;
				m_binsWidth = new FeatType[m_numDimensions];
	
				for (int d=0; d<m_numDimensions; d++) {
					in >> m_binsWidth[d];
				}
			}
			else if (tag == "Bins") {
				// Allocate memory
				delete [] m_bins;
				m_bins = new BinType[m_totalBins];
	
				for (int b=0; b<m_totalBins; b++) {
					in >> m_bins[b];
				}
			}
			else {
				throw new FD::GeneralException ("VisualHistogramDesc::readFrom : Unknown argument: " + tag,__FILE__,__LINE__);
			}
	
			if (!in) {
				throw new FD::GeneralException ("VisualHistogramDesc::readFrom : Parse error trying to build " + tag,__FILE__,__LINE__);
			}
	
			in >> tag;
			if (tag != ">") {
				throw new FD::GeneralException ("VisualHistogramDesc::readFrom : Parse error: '>' expected ",__FILE__,__LINE__);
			}
		}
	}

	double Similarity(const BinType *i_candidate, unsigned int i_size) const
	{
		try {
			if (m_totalBins != i_size ) {
				throw new FD::GeneralException ("VisualHistogramDesc::Similarity : number of histogram bins differs from current descriptor",__FILE__,__LINE__);
			}
			
			if (!m_valid) {
				// Invalid descriptor, output maximal distance
				return 0.0;
			}
			
			// Use appropriate function
			return (this->*m_similarityFct)(i_candidate);
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualHistogramDesc::Similarity:",__FILE__,__LINE__));
		}
	}
	
	void Adapt(const BinType *i_candidate, unsigned int i_size, double i_rate)
	{
		try {
			if (m_totalBins != i_size ) {
				throw new FD::GeneralException ("VisualHistogramDesc::Adapt : number of histogram bins differs from current descriptor",__FILE__,__LINE__);
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
			
			// Use appropriate function
			(this->*m_adaptFct)(i_candidate, i_rate);
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualHistogramDesc::Adapt:",__FILE__,__LINE__));
		}
	}
	
	void ComputeHistStd(const FeatType* i_featMap, FeatType i_maxValPlusOne,
		BinType i_binIncScale, int i_width, int i_height, const VisualROI *i_roi)
	{
		if (!m_bins) {
			throw new FD::GeneralException ("VisualHistogramDesc::ComputeHistStd : histogram bins must be initialized.",__FILE__,__LINE__);
		}
		
		int hx=i_roi->GetHSX(), hy=i_roi->GetHSY();
		int xCen = i_roi->GetXCen();
		int yCen = i_roi->GetYCen();
		const FeatType *p_features;
		const unsigned char *p_mask = i_roi->GetCstMask();
		int val, fact, indx, maskLeftOffset, maskRightOffset;
		int x, y, i, j, numFeatures, xLeftOffset, xRightOffset, yTopOffset, yBottomOffset;
		BinType weightSum = (BinType)0;
		
		if (m_lastMaxVal != i_maxValPlusOne) {
			m_lastMaxVal = i_maxValPlusOne;
			
			for (i=0; i<m_numDimensions; ++i) {
				m_binsWidth[i] = (FeatType)((double)i_maxValPlusOne/(double)m_numBins[i] + 0.5);
			}
		}
	
		for (i=0; i<m_totalBins; ++i) {
			m_bins[i] = (BinType)(0);
		}
	
		// Adjust region offsets to fit in feature map only
		if (xCen-hx < 0) {
			xLeftOffset = 0;
			maskLeftOffset = hx-xCen;
		}
		else {
			xLeftOffset = xCen-hx;
			maskLeftOffset = 0;
		}
	
		if (xCen+hx > i_width-1) {
			xRightOffset = i_width-1;
			maskRightOffset = xCen+hx - i_width-1;
		}
		else {
			xRightOffset = xCen+hx;
			maskRightOffset = 0;
		}
	
		if (yCen-hy < 0) {
			yTopOffset = 0;
			p_mask += (hy-yCen)*(2*hx+1);
		}
		else {
			yTopOffset = yCen-hy;
		}
	
		if (yCen+hy > i_height-1) {
			yBottomOffset = i_height-1;
		}
		else {
			yBottomOffset = yCen+hy;
		}
	
		p_features = (const FeatType *)(i_featMap + m_numDimensions*(yTopOffset*i_width + xLeftOffset));
	
		// For each pixels in the region of interest, compute the histogram bin index
		// then add "1" to this bin.
		for (y=yTopOffset; y<=yBottomOffset; y++) {
			numFeatures = 0;
			p_mask += maskLeftOffset;
	
			for (x=xLeftOffset; x<=xRightOffset; x++) {
				if (*p_mask++) {
					indx = 0;
					
					for (i=0; i<m_numDimensions; ++i) {
						fact = 1;
	
						val = (*p_features)/m_binsWidth[i];
						p_features++;
	
						for (j=i; j>0; --j) {
							fact *= m_numBins[j-1];
						}
	
						indx += val*fact;
					}
	
					if (indx < 0 || indx >= m_totalBins) {
						throw new FD::GeneralException ("VisualHistogramDesc::ComputeHistStd : invalid histogram bin index",__FILE__,__LINE__);
					}
	
					m_bins[indx]++;
					weightSum++;
				}
				else {
					p_features += m_numDimensions;
				}
	
				numFeatures++;
			}
	
			p_features += m_numDimensions*(i_width-numFeatures);
			p_mask += maskRightOffset;
		}
	
		if (m_normFlag) {
			NormalizeBins(weightSum);
		}
	}
	
	void ComputeKernelWeightedHist(const FeatType* i_featMap, FeatType i_maxValPlusOne,
		BinType i_binIncScale, int i_width, int i_height, const VisualROI *i_roi)
	{
		if (!m_bins) {
			throw new FD::GeneralException ("VisualHistogramDesc::ComputeKernelWeightedHist : histogram bins must be initialized.",__FILE__,__LINE__);
		}
		
		int hx=i_roi->GetHSX(), hy=i_roi->GetHSY();
		int xCen = i_roi->GetXCen();
		int yCen = i_roi->GetYCen();
		const FeatType *p_features;
		const unsigned char *p_mask = i_roi->GetCstMask();
		int val, fact, indx, maskLeftOffset, maskRightOffset;
		int x, y, i, j, numFeatures, xLeftOffset, xRightOffset, yTopOffset, yBottomOffset;
		double xnorm, ynorm, weight;
		BinType weightSum = (BinType)0;
		
		if (m_lastMaxVal != i_maxValPlusOne) {
			m_lastMaxVal = i_maxValPlusOne;
			
			for (i=0; i<m_numDimensions; ++i) {
				m_binsWidth[i] = (FeatType)round((double)i_maxValPlusOne/(double)m_numBins[i] + 0.5);
			}
		}
	
		for (i=0; i<m_totalBins; ++i) {
			m_bins[i] = (BinType)(0);
		}
	
		// Adjust region offsets to fit in feature map only
		if (xCen-hx < 0) {
			xLeftOffset = 0;
			maskLeftOffset = hx-xCen;
		}
		else {
			xLeftOffset = xCen-hx;
			maskLeftOffset = 0;
		}
	
		if (xCen+hx > i_width-1) {
			xRightOffset = i_width-1;
			maskRightOffset = xCen+hx - i_width-1;
		}
		else {
			xRightOffset = xCen+hx;
			maskRightOffset = 0;
		}
	
		if (yCen-hy < 0) {
			yTopOffset = 0;
			p_mask += (hy-yCen)*(2*hx+1);
		}
		else {
			yTopOffset = yCen-hy;
		}
	
		if (yCen+hy > i_height-1) {
			yBottomOffset = i_height-1;
		}
		else {
			yBottomOffset = yCen+hy;
		}
	
		p_features = (const FeatType *)(i_featMap + m_numDimensions*(yTopOffset*i_width + xLeftOffset));
	
		// For each pixels in the region of interest, compute the histogram bin index
		// then compute this pixel weight base on its distance from the center of the
		// region using a kernel profile. This weight will be added in the appropriate
		// histogram bin.
		for (y=yTopOffset; y<=yBottomOffset; y++) {
			numFeatures = 0;
			p_mask += maskLeftOffset;
	
			for (x=xLeftOffset; x<=xRightOffset; x++) {
				if (*p_mask++) {
					indx = 0;
					
					for (i=0; i<m_numDimensions; ++i) {
						fact = 1;
	
						val = (*p_features)/m_binsWidth[i];
						p_features++;
	
						for (j=i; j>0; --j) {
							fact *= m_numBins[j-1];
						}
	
						indx += val*fact;
					}
	
					if (indx < 0 || indx >= m_totalBins) {
						throw new FD::GeneralException ("VisualHistogramDesc::ComputeKernelWeightedHist : invalid histogram bin index",__FILE__,__LINE__);
					}
	
					// Compute normalized distance of the pixel from the center
					// assuming a unit circle region.
					xnorm = (double)(x-xCen)/(double)(hx);
					ynorm = (double)(y-yCen)/(double)(hy);
	
					// Using by default the Epanechnikov kernel
					// TODO: make kernel profiles enum to choose from
					weight = EpanechKernel(xnorm*xnorm + ynorm*ynorm);
					BinType histWeight = (BinType)(weight*(double)(i_binIncScale));
					weightSum += histWeight;
	
					m_bins[indx] += histWeight;
				}
				else {
					p_features += m_numDimensions;
				}
	
				numFeatures++;
			}
	
			p_features += m_numDimensions*(i_width-numFeatures);
			p_mask += maskRightOffset;
		}
	
		if (m_normFlag && weightSum) {
			NormalizeBins(weightSum);
		}
	}
	
	void ComputeMSLocation(const FeatType* i_featMap, FeatType i_maxValPlusOne,
		BinType i_binIncScale, int i_width, int i_height, const VisualROI *i_roi,
		const BinType *i_refBins, double i_cueWeight, FD::Vector<double> *o_msLoc)
	{
		if (!m_bins) {
			throw new FD::GeneralException ("VisualHistogramDesc::ComputeMSLocation : histogram bins must be initialized.",__FILE__,__LINE__);
		}
		
		int hx=i_roi->GetHSX(), hy=i_roi->GetHSY();
		int xCen = i_roi->GetXCen();
		int yCen = i_roi->GetYCen();
		const FeatType *p_features;
		const unsigned char *p_mask = i_roi->GetCstMask();
		int val, fact, indx, maskLeftOffset, maskRightOffset;
		int x, y, i, j, numFeatures, xLeftOffset, xRightOffset, yTopOffset, yBottomOffset;
		double xnorm, ynorm;
		double tmpRef, tmpCand, posWeight, kernWeight;
		double numeratorX=0.0, numeratorY=0.0, denominator=0.0;
	
		// Adjust region offsets to fit in feature map only
		if (xCen-hx < 0) {
			xLeftOffset = 0;
			maskLeftOffset = hx-xCen;
		}
		else {
			xLeftOffset = xCen-hx;
			maskLeftOffset = 0;
		}
	
		if (xCen+hx > i_width-1) {
			xRightOffset = i_width-1;
			maskRightOffset = xCen+hx - i_width-1;
		}
		else {
			xRightOffset = xCen+hx;
			maskRightOffset = 0;
		}
	
		if (yCen-hy < 0) {
			yTopOffset = 0;
			p_mask += (hy-yCen)*(2*hx+1);
		}
		else {
			yTopOffset = yCen-hy;
		}
	
		if (yCen+hy > i_height-1) {
			yBottomOffset = i_height-1;
		}
		else {
			yBottomOffset = yCen+hy;
		}
	
		p_features = (const FeatType *)(i_featMap + m_numDimensions*(yTopOffset*i_width + xLeftOffset));

		for (y=yTopOffset; y<=yBottomOffset; y++) {
			numFeatures = 0;
			p_mask += maskLeftOffset;
	
			for (x=xLeftOffset; x<=xRightOffset; x++) {
				if (*p_mask++) {
					indx = 0;
					
					for (i=0; i<m_numDimensions; ++i) {
						fact = 1;
	
						val = (*p_features)/m_binsWidth[i];
						p_features++;
	
						for (j=i; j>0; --j) {
							fact *= m_numBins[j-1];
						}
	
						indx += val*fact;
					}
	
					if (indx < 0 || indx >= m_totalBins) {
						throw new FD::GeneralException ("VisualHistogramDesc::ComputeMSLocation : invalid histogram bin index",__FILE__,__LINE__);
					}
					
					// Compute normalized distance of the pixel from the center
					// assuming a unit circle region.
					xnorm = (double)(x-xCen)/(double)(hx);
					ynorm = (double)(y-yCen)/(double)(hy);
					
					tmpRef = (double)(i_refBins[indx]);
					tmpCand = (double)(m_bins[indx]);
					
					if (tmpCand > 0.0) {
						posWeight = sqrt(tmpRef/tmpCand);
						kernWeight = DerivedEpanechKernel(xnorm*xnorm + ynorm*ynorm);
						
						numeratorX += posWeight*kernWeight*x;
						numeratorY += posWeight*kernWeight*y;
						denominator += posWeight*kernWeight;
					}
				}
				else {
					p_features += m_numDimensions;
				}
	
				numFeatures++;
			}
	
			p_features += m_numDimensions*(i_width-numFeatures);
			p_mask += maskRightOffset;
		}
	
		if (denominator > 0.0) {
			// Mean shift x location
			(*o_msLoc)[0] = i_cueWeight*(numeratorX/denominator);
			// Mean shift y location
			(*o_msLoc)[1] = i_cueWeight*(numeratorY/denominator);
		}
		else {
			// Invalid mean shift estimation. 
			// Output current ROI center location.
			// Mean shift x location
			(*o_msLoc)[0] = xCen;
			// Mean shift y location
			(*o_msLoc)[1] = yCen;
		}
	}
	
	unsigned int GetNumDimensions() const
	{
		return m_numDimensions;
	}

	unsigned int GetSize() const
	{
		return m_totalBins;
	}

	unsigned int *GetNumBinsPtr() 
	{
		return (const unsigned int *)m_numBins;
	}
	
	const unsigned int *GetCstNumBinsPtr() const
	{
		return (const unsigned int *)m_numBins;
	}
	
	FeatType *GetBinsWidthPtr() 
	{
		return m_binsWidth;
	}
	
	const FeatType *GetCstBinsWidthPtr() const
	{
		return (const FeatType *)m_binsWidth;
	}

	BinType *GetFeatures()
	{
		return m_bins;
	}
	
	const BinType *GetCstFeatures() const
	{
		return (const BinType *)m_bins;
	}
	
	bool GetValidity() const
	{
		return m_valid;
	}
	
	// WARNING: this function modifies only the histogram bins
	void SetSize(unsigned int i_size)
	{
		delete [] m_bins;
		m_totalBins = i_size;
		m_bins = new BinType[m_totalBins];
	}
	
	// A more complete Set function for the histogram bins
	void SetBins(unsigned int i_numDimensions, 
		unsigned int *i_numBins)
	{
		m_numDimensions = i_numDimensions;
		unsigned int totalBins = 1;
		
		delete [] m_numBins;
		m_numBins = new unsigned int[m_numDimensions];
		
		delete [] m_binsWidth;
		m_binsWidth = new FeatType[m_numDimensions];
		m_lastMaxVal = (FeatType)0;

		for(int d=0; d<m_numDimensions; d++) {
			m_numBins[d] = i_numBins[d];
			totalBins *= m_numBins[d];
		}
		
		if (totalBins != m_totalBins) {
			SetSize(totalBins);
		}
	}
	
	// WARNING: assumes that i_ref has the same size as the current object
	void SetFeatures(const BinType *i_ref, unsigned int i_size)
	{
		if (m_totalBins != i_size ) {
			throw new FD::GeneralException ("VisualHistogramDesc::SetFeatures : number of histogram bins differs from current descriptor",__FILE__,__LINE__);
		}
		
		for (int b=0; b<m_totalBins; b++) {
			m_bins[b] = i_ref[b];
		}
	}
	
	void SetValidity(bool i_flag)
	{
		m_valid = i_flag;
	}

private:
	void SetSimilarityFct()
	{
		if (m_simType == e_VISUALHIST_BhattacharyyaCoeff) {
			m_similarityFct = &VisualHistogramDesc::BhattacharyyaCoeff;
		}
		else if (m_simType == e_VISUALHIST_BhattacharyyaDist) {
			m_similarityFct = &VisualHistogramDesc::BhattacharyyaDist;
		}
		else if (m_simType == e_VISUALHIST_HistIntersection) {
			if (m_normFlag) {
				m_similarityFct = &VisualHistogramDesc::HistogramIntersection;
			}
			else {
				m_similarityFct = &VisualHistogramDesc::HistogramIntersectionWNorm;
			}
		}
		else if (m_simType == e_VISUALHIST_EuclideanDist) {
			m_similarityFct = &VisualHistogramDesc::EuclideanDist;
		}
		else {
			m_similarityFct = NULL;
		}
	}
	
	void SetAdaptFct()
	{
		if (m_normFlag) {
			m_adaptFct = &VisualHistogramDesc::AdaptNorm;
		}
		else {
			m_adaptFct = &VisualHistogramDesc::AdaptOnly;
		}
	}
	
	double BhattacharyyaDist(const BinType *i_candidateBins) const
	{
		try {
			return sqrt(1.0-BhattacharyyaCoeff(i_candidateBins));
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception caught in VisualHistogramDesc::BhattacharyyaDist:",__FILE__,__LINE__));
		}
	}
	
	double BhattacharyyaCoeff(const BinType *i_candidateBins) const
	{
		int b;
		double coeff = 0.0;
		const BinType *p_curBins = (const BinType *)m_bins;
		const BinType *p_candidateBins = i_candidateBins;
	
		for (int b=0; b<m_totalBins; b++) {
			coeff += sqrt((*p_curBins) * (*p_candidateBins));
			p_curBins++;
			p_candidateBins++;
		}
	
		//return coeff/sqrt((double)m_totalBins);
		return coeff;
	}
	
	double HistogramIntersection(const BinType *i_candidateBins) const
	{
		const BinType *p_curBins = (const BinType *)m_bins;
		const BinType *p_candidateBins = i_candidateBins;
		double sum = 0.0;
		
		for (int b=0; b<m_totalBins; b++) {
			if (*p_curBins < *p_candidateBins) {
				sum += *p_curBins;
			}
			else {
				sum += *p_candidateBins;
			}

			p_curBins++;
			p_candidateBins++;
		}
		
		return 1.0-sum;
	}
	
	double HistogramIntersectionWNorm(const BinType *i_candidateBins) const
	{
		const BinType *p_curBins = (const BinType *)m_bins;
		const BinType *p_candidateBins = i_candidateBins;
		BinType curHistSum = (BinType)0;
		BinType candHistSum = (BinType)0;
		BinType curNormVal;
		BinType candNormVal;
		double sum = 0.0;
		
		for (int b=0; b<m_totalBins; b++) {
			curHistSum += *p_curBins;
			candHistSum += *p_candidateBins;
			
			p_curBins++;
			p_candidateBins++;
		}
		
		p_curBins = (const BinType *)m_bins;
		p_candidateBins = i_candidateBins;
		
		for (int b=0; b<m_totalBins; b++) {
			curNormVal = (*p_curBins)/curHistSum;
			candNormVal = (*p_candidateBins)/candHistSum;
			
			if (curNormVal < candNormVal) {
				sum += curNormVal;
			}
			else {
				sum += candNormVal;
			}

			p_curBins++;
			p_candidateBins++;
		}
		
		return 1.0-sum;
	}
	
	double EuclideanDist(const BinType *i_candidateBins) const
	{
		int b;
		BinType diff;
		double dist = 0.0;
		const BinType *p_curBins = (const BinType *)m_bins;
		const BinType *p_candidateBins = i_candidateBins;
	
		for (int b=0; b<m_totalBins; b++) {
			diff = (*p_curBins) - (*p_candidateBins);
			dist += diff*diff;
			p_curBins++;
			p_candidateBins++;
		}
		
		dist = sqrt(dist);
	
		return 1.0-dist;
	}
	
	void AdaptNorm(const BinType *i_candidateBins, double i_rate)
	{
		int b;
		const BinType *p_adaptBins = i_candidateBins;
		double compRate = 1.0 - i_rate;
		BinType sum = (BinType)0;
		
		for (b=0; b<m_totalBins; b++) {
			m_bins[b] = (BinType)(compRate*(double)(m_bins[b]) + i_rate*(double)(p_adaptBins[b]));
			sum += m_bins[b];
		}
	
		if (sum != (BinType)0) {
			NormalizeBins(sum);
		}
	}
	
	void AdaptOnly(const BinType *i_candidateBins, double i_rate)
	{
		int b;
		const BinType *p_adaptBins = i_candidateBins;
		double compRate = 1.0 - i_rate;
		
		for (b=0; b<m_totalBins; b++) {
			m_bins[b] = (BinType)(compRate*(double)(m_bins[b]) + i_rate*(double)(p_adaptBins[b]));
		}
	}
	
	void NormalizeBins(BinType i_fact)
	{
		int b;
		double invFact = 1.0/(double)i_fact;
	
		for (b=0; b<m_totalBins; ++b) {
			m_bins[b] = (BinType)((double)(m_bins[b])*invFact);
		}
	}
	
	inline double EpanechKernel(double i_dist) const
	{
		return std::max( 0.0, k_VISUALHIST_2DIVPI * (1.0 - i_dist) );
	}
	
	inline double DerivedEpanechKernel(double i_dist) const
	{
		if (i_dist <= 1.0) {
			return 1.0;
		}
		
		return 0.0;
	}
	
private:
	// Similarity/Distance type to use
	e_VISUALHIST_similarityType m_simType;
	
	// Validity flag in cases where ROI was invalid for features extraction
	bool m_valid;
	
	// Flag to normalize histogram bins
	bool m_normFlag;
	
	// Numer of dimensions of the histogram
	unsigned int m_numDimensions;
	
	// Total number of bins in the histogram
	unsigned int m_totalBins;
	
	// Number of bins for each dimension
	int *m_numBins;
	
	// Width (in terms of the feature type) of each bin
	FeatType *m_binsWidth;
	
	// Last maximum value used to compute the width of each bin
	FeatType m_lastMaxVal;
	
	// Histogram bin data
	BinType *m_bins;
	
	// Function pointer to the appropriate private similarity routine
	double (VisualHistogramDesc::*m_similarityFct)(const BinType *) const;
	
	// Function pointer to the appropriate private adaptation routine
	void (VisualHistogramDesc::*m_adaptFct)(const BinType *, double);
};

}

#endif
