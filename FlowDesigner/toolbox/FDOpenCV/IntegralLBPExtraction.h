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
#ifndef _INTEGRALLBPEXTRACTION_H_
#define _INTEGRALLBPEXTRACTION_H_

#include "VisualFeaturesExtraction.h"
#include "VisualIntegralDesc.h"
#include "VisualTarget.h"
#include <stdlib.h>
#include <sys/timeb.h>

namespace RobotFlow {
//
// Integral LBP (Lcoal Binary Pattern) Features Extraction for RobotFlow 
//
class IntegralLBPExtraction : public VisualFeaturesExtraction<double>
{ 
public:
	//
	// Default constructor for Object 
	//
	IntegralLBPExtraction();
	
	//
	// Constructor with complete intialisation
	//
	IntegralLBPExtraction(int i_width, int i_height, 
		int i_numChannels, int i_numHoriIntRect, int i_numVertIntRect,
		int i_numSamples, int i_predicate, bool i_doInterpolation,
		bool i_useUniform, int i_startAngle, double i_maxValue,
		bool i_useRectDiff);

	//
	// BufferedNode constructor
	//
	IntegralLBPExtraction(std::string nodeName, FD::ParameterSet params);

	//
	// Constructor using input stream
	//
	IntegralLBPExtraction(std::istream &in)
	{
		readFrom(in);
	}
	
	virtual ~IntegralLBPExtraction();
	
	// Default routine to print a IntegralLBPExtraction object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in IntegralLBPExtraction::printOn: method not yet implemented.",__FILE__,__LINE__);
	}

	// Default routine to read a IntegralLBPExtraction object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in IntegralLBPExtraction::readFrom: method not yet implemented.",__FILE__,__LINE__);
	}
	
	virtual void request(int output_id, const FD::ParameterSet &req);
	
	void calculate(int output_id, int count, FD::Buffer &out);
	
	void Preprocess(IplImage *i_src);
	
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
	
	void InitSamplePoints();
	
	void ExtractLBPGeneralWithInterpolation(unsigned char *i_pixPtr);
	
	void ExtractLBPGeneralWithoutInterpolation(unsigned char *i_pixPtr);
	
	void ExtractLBPGeneralRIU2WithInterpolation(unsigned char *i_pixPtr);
	
	void ExtractLBPGeneralRIU2WithoutInterpolation(unsigned char *i_pixPtr);
	
	void ExtractLBP8WithInterpolation(unsigned char *i_pixPtr);
	
	void ExtractLBP8WithoutInterpolation(unsigned char *i_pixPtr);
	
	void ExtractLBP8RIU2WithInterpolation(unsigned char *i_pixPtr);
	
	void ExtractLBP8RIU2WithoutInterpolation(unsigned char *i_pixPtr);
	
	int ComputeBitTransitions(unsigned int i_val);
	
	int CountOneBits(unsigned int i_val);

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
	// The number of samples in the local neighborhood.
	int m_numSamples;
	// The current predicate (radius), i.e. the distance of the
	//  neighborhood from its center.
	int m_predicate;
	// Interpolation flag
	bool m_doInterpolation;
	// Flag to use only uniform patterns i.e. patterns with 2 or less
	//  bit transitions
	bool m_useUniform;
	// The angle of the first neighbor.
	int m_startAngle;
	// Maximum pixel channel value
	double m_maxValue;
	// Number of valid local binary patterns
	int m_numValidPattern;
	
	double m_maxFeatValue;
	
	bool m_useRectDiff;
	double *m_tmpMeanFeatures;
	double *m_curMeanVal;
	
	// Precalculated table of interpolation points.
	CvPoint *m_samplePoints;
	// Precalculated table of interpolation offsets.
	CvPoint2D32f *m_pointsOffsets;
	// Precalculated values for interpolation multiplication.
	double *m_BiLiMultipliers;
	// Temporary pixel pointers corresponding to the neighborhood samples
	unsigned char **m_tmpSamples;
	
	// Integral color descriptor for region of interest
	FD::RCPtr<FD::Vector<VisualFeatureDesc<double> *> > m_featVect;
	
	// Temporary image copy
	IplImage *m_curImage;
	// Grayscale version of current image
	IplImage *m_grayImage;
	// Each local pattern is an image
	IplImage **m_patternImage;
	// Pointer to channel image pixels
	unsigned char **m_patternPixPtr;
	int **m_sumPixPtr;
	// Sum of pixels (integral) image
	IplImage **m_sumImage;
	
	// Function pointer to the appropriate private extraction routine
	void (IntegralLBPExtraction::*m_extractionFct)(unsigned char *);
};

}

#endif
