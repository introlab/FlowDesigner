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
#ifndef _COLORHISTEXTRACTION_H_
#define _COLORHISTEXTRACTION_H_

#include "VisualFeaturesExtraction.h"
#include "VisualHistogramDesc.h"
#include "VisualTarget.h"
#include <stdlib.h>
#include <sys/timeb.h>

namespace RobotFlow {
//
// Color histogram Features Extraction for RobotFlow 
//
class ColorHistExtraction : public VisualFeaturesExtraction<double>
{ 
public:
	//
	// Default constructor for Object 
	//
	ColorHistExtraction();
	
	//
	// Constructor with complete intialisation
	//
	ColorHistExtraction(int i_width, int i_height, 
		int i_numChannels, const FD::Vector<int> *i_numBins);

	//
	// Copy constructor
	//
	ColorHistExtraction(const ColorHistExtraction& i_ref);

	//
	// BufferedNode constructor
	//
	ColorHistExtraction(std::string nodeName, FD::ParameterSet params);

	//
	// Constructor using input stream
	//
	ColorHistExtraction(std::istream &in)
	{
		readFrom(in);
	}
	
	virtual ~ColorHistExtraction();
	
	// Default routine to print a ColorHistExtraction object to an output stream
	void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in ColorHistExtraction::printOn: method not yet implemented.",__FILE__,__LINE__);
	}

	// Default routine to read a ColorHistExtraction object from an input stream
	void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in ColorHistExtraction::readFrom: method not yet implemented.",__FILE__,__LINE__);
	}
	
	virtual void request(int output_id, const FD::ParameterSet &req);
	
	void calculate(int output_id, int count, FD::Buffer &out);
	
	void ExtractFeatures(VisualROI *i_roi);
	
	void ExtractFeatures(IplImage *i_input, VisualROI *i_roi);
	
	void EstimateMSLocation(const VisualTarget<double> *i_targetRef, 
		int i_descIdx, FD::Vector<double> *o_msLocVec);
	
	VisualFeatureDesc<double> *GetDescriptor()
	{
		return m_colorHistogram;
	}
	
	const VisualFeatureDesc<double> *GetCstDescriptor() const
	{
		return (const VisualHistogramDesc<double, unsigned char> *)m_colorHistogram;
	}

private:
	void Initialize(const FD::Vector<int> *i_numBins);

private:
	// Input IDs (for BufferedNode)
	int m_imageInID;
	int m_numBinsInID;
	int m_roiInID;
	int m_targetInID;
	int m_targetDescIdxInID;
	int m_useNextImgInID;
	
	// Output IDs (for BufferedNode)
	int m_featuresOutID;
	int m_msLocOutID;
	int m_ppCompletedOutID;
	
	// Width of images
	int m_width;
	// Height of images
	int m_height;
	// Number of channels in an image
	int m_numChannels;
	// Number of pixels in an image
	int m_numPixels;
	// Number of bytes in an image
	int m_numBytesInFrame;
	
	// Initialization flag
	bool m_init;
	
	// Histogram descriptor for region of interest
	VisualHistogramDesc<double, unsigned char> *m_colorHistogram; 
	
	// Function pointer to the appropriate private extraction routine
	void (ColorHistExtraction::*m_extractionFct)(unsigned char *);
	
	// Temporary image copy
	IplImage *m_curImage;
};

}

#endif
