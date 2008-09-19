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
#ifndef _INTEGRALEDGESORIEXTRACTION_H_
#define _INTEGRALEDGESORIEXTRACTION_H_

#include "VisualFeaturesExtraction.h"
#include "VisualIntegralDesc.h"
#include "VisualTarget.h"
#include <stdlib.h>
#include <sys/timeb.h>

namespace RobotFlow {
//
// Integral Edges Orientation Features Extraction for RobotFlow 
//
class IntegralEdgesOriExtraction : public VisualFeaturesExtraction<double>
{ 
public:
	//
	// Default constructor for Object 
	//
	IntegralEdgesOriExtraction();
	
	//
	// Constructor with complete intialisation
	//
	IntegralEdgesOriExtraction(int i_width, int i_height, 
		int i_numChannels, int i_numHoriIntRect, int i_numVertIntRect,
		int i_numOriBins, double i_edgesStrTresh,
		double i_maxStrengthValue, bool i_useRectDiff);

	//
	// BufferedNode constructor
	//
	IntegralEdgesOriExtraction(std::string nodeName, FD::ParameterSet params);

	//
	// Constructor using input stream
	//
	IntegralEdgesOriExtraction(std::istream &in)
	{
		readFrom(in);
	}
	
	virtual ~IntegralEdgesOriExtraction();
	
	// Default routine to print a IntegralEdgesOriExtraction object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in IntegralEdgesOriExtraction::printOn: method not yet implemented.",__FILE__,__LINE__);
	}

	// Default routine to read a IntegralEdgesOriExtraction object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in IntegralEdgesOriExtraction::readFrom: method not yet implemented.",__FILE__,__LINE__);
	}
	
	virtual void request(int output_id, const FD::ParameterSet &req);
	
	void calculate(int output_id, int count, FD::Buffer &out);
	
	void Preprocess(IplImage *i_srcImg);
	
	void ExtractFeatures(VisualROI *i_roi);
	
	void ExtractFeatures(IplImage *i_input, VisualROI *i_roi);
	
	VisualFeatureDesc<double> *GetDescriptor()
	{
		return (*m_featVect)[0];
	}
	
	const VisualFeatureDesc<double> *GetCstDescriptor() const
	{
		return (const VisualIntegralDesc<double> *)(*m_featVect)[0];
	}
	
	FD::ObjectRef GetEdgesOriSumRef() 
	{
		return m_edgesOriSumRef;
	}
	
	void SetEdgesOriSumRef(FD::ObjectRef i_ref) 
	{
		m_edgesOriSumRef = i_ref;
		
		if (!m_edgesOriSumRef->isNil()) {
			FD::RCPtr<FD::Vector<double *> > imgVecRef = m_edgesOriSumRef;
			if (imgVecRef->size() != m_numOriBins) {
				throw new FD::GeneralException("Exception in IntegralEdgesOriExtraction::SetEdgesOriSumRef: given reference does not seem to have the same number of orientations.",__FILE__,__LINE__);
			}
		}
	}

private:
	void Initialize();

private:
	// Input IDs (for BufferedNode)
	int m_imageInID;
	int m_roiInID;
	int m_useNextImgInID;
	
	// Output IDs (for BufferedNode)
	int m_featuresOutID;
	int m_ppCompletedOutID;
	
	// Width of images
	int m_width;
	int m_imgSumWidth;
	// Height of images
	int m_height;
	// Number of channels in an image
	int m_numChannels;
	// Number of pixels in an image
	int m_numPixels;
	// Number of bytes in an image
	int m_numBytesInFrame;
	// Number of orientations to use
	int m_numOriBins;
	// Number of independant rectangular region to compute
	// the integral edges orientation features
	int m_numHoriIntRect;
	int m_numVertIntRect;
	int m_numIntRect;
	// Edge strength threshold to remove noisy edges
	double m_edgesStrTresh;
	// Maximum strength channel value
	double m_maxStrengthValue;
	double m_maxFeatValue;
	
	bool m_useRectDiff;
	double *m_tmpMeanFeatures;
	double *m_curMeanVal;
	
	// Integral color descriptor for region of interest
	FD::RCPtr<FD::Vector<VisualFeatureDesc<double> *> > m_featVect;
	
	// Temporary image copy
	IplImage *m_curImage;
	// Grayscale version of current image
	IplImage *m_grayImage;
	// Result from filtering With Sobel (X)
	IplImage *m_oriXImage;
	// Result from filtering With Sobel (Y)
	IplImage *m_oriYImage;
	// Multi-channel edges orientation map
	IplImage **m_edgesOri;
	// Multi-channel sum of edges orientation map
	IplImage **m_edgesOriSum;
	
	FD::ObjectRef m_edgesOriSumRef;
	
	// Pixel/value pointers
	float **m_edgesOriPix;
	double **m_edgesOriSumPix;
};

}

#endif
