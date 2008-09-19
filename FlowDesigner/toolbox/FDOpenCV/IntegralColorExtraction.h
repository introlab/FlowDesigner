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
#ifndef _INTEGRALCOLOREXTRACTION_H_
#define _INTEGRALCOLOREXTRACTION_H_

#include "VisualFeaturesExtraction.h"
#include "VisualIntegralDesc.h"
#include "VisualTarget.h"
#include <stdlib.h>
#include <sys/timeb.h>

namespace RobotFlow {
//
// Integral Color Features Extraction for RobotFlow 
//
class IntegralColorExtraction : public VisualFeaturesExtraction<double>
{ 
public:
	//
	// Default constructor for Object 
	//
	IntegralColorExtraction();
	
	//
	// Constructor with complete intialisation
	//
	IntegralColorExtraction(int i_width, int i_height, 
		int i_numChannels, int i_numHoriIntRect, int i_numVertIntRect,
		double i_maxValue, bool i_useRectDiff, bool i_useBoundary, 
		double i_boundaryMeanDiffThresh);

	//
	// BufferedNode constructor
	//
	IntegralColorExtraction(std::string nodeName, FD::ParameterSet params);

	//
	// Constructor using input stream
	//
	IntegralColorExtraction(std::istream &in)
	{
		readFrom(in);
	}
	
	virtual ~IntegralColorExtraction();
	
	// Default routine to print a IntegralColorExtraction object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in IntegralColorExtraction::printOn: method not yet implemented.",__FILE__,__LINE__);
	}

	// Default routine to read a IntegralColorExtraction object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in IntegralColorExtraction::readFrom: method not yet implemented.",__FILE__,__LINE__);
	}
	
	virtual void request(int output_id, const FD::ParameterSet &req);
	
	void calculate(int output_id, int count, FD::Buffer &out);
	
	void Preprocess(IplImage *i_srcImg);
	
	void Preprocess(const unsigned char *i_src);
	
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
	// Number of independant rectangular region to compute
	// the integral color features
	int m_numHoriIntRect;
	int m_numVertIntRect;
	int m_numIntRect;
	int m_numFeatures;
	// Maximum pixel channel value
	double m_maxValue;
	
	bool m_useRectDiff;
	double *m_tmpMeanFeatures;
	double *m_curMeanVal;
	
	bool m_useBoundary;
	double m_boundaryMeanDiffThresh;
	
	// Integral color descriptor for region of interest
	FD::RCPtr<FD::Vector<VisualFeatureDesc<double> *> > m_featVect;
	
	// Temporary image copy
	IplImage *m_curImage;
	// Each channel image
	IplImage **m_chImage;
	// Pointer to channel image pixels
	unsigned char **m_chPixPtr;
	int **m_sumPixPtr;
	// Sum of pixels (integral) image
	IplImage **m_sumImage;
};

}

#endif
