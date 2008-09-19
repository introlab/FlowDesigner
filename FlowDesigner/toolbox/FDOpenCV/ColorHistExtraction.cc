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
#include "ColorHistExtraction.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

DECLARE_NODE(ColorHistExtraction)
DECLARE_TYPE(ColorHistExtraction)

  /*Node
   *
   * @name ColorHistExtraction
   * @category RobotFlow:Vision:FeaturesExtraction
   * @description Color histogram features extraction.
   *
   * @parameter_name FRAME_WIDTH
   * @parameter_type int
   * @parameter_value 320
   * @parameter_description Video frame width.
   *
   * @parameter_name FRAME_HEIGHT
   * @parameter_type int
   * @parameter_value 240
   * @parameter_description Video frame height.
   *
   * @parameter_name NUM_CHANNELS
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description Number of channels in video frame.
   *
   * @input_name IN_IMAGE
   * @input_type Image
   * @input_description Current frame to process.
   *
   * @input_name NUM_BINS
   * @input_type Vector<int>
   * @input_description Number of bins for each dimension of the histogram.
   *
   * @input_name CURRENT_ROI
   * @input_type VisualROI
   * @input_description The current region of interest.
   *
   * @input_name CURRENT_TARGET
   * @input_type VisualTarget
   * @input_description The current target to estimate the mean shif location.
   *
   * @input_name TARGET_DESC_IDX
   * @input_type int
   * @input_description The corresponding descriptor index in the current target descriptors vector.
   *
   * @input_name PREPROCESS_FRAME
   * @input_type bool
   * @input_description Flag indicating to preprocess a new image.
   *
   * @output_name OUT_FEATURES
   * @output_type Vector<VisualFeatureDesc<double> *>
   * @output_description Output features descriptor.
   *
   * @output_name OUT_MS_LOCATION
   * @output_type Vector<double>
   * @output_description Mean shift vector of location.
   *
   * @output_name PREPROCESS_COMPLETED
   * @output_type int
   * @output_description Output to force preprocessing.
   *
   END*/
	
   
//
// Default constructor for Object 
//
ColorHistExtraction::ColorHistExtraction()
: m_width(-1),
m_height(-1),
m_numChannels(-1),
m_init(false),
m_colorHistogram(NULL)
{

}

//
// Constructor with complete intialisation
//
ColorHistExtraction::ColorHistExtraction(int i_width, int i_height, 
	int i_numChannels, const Vector<int> *i_numBins)
: m_width(i_width),
m_height(i_height),
m_numChannels(i_numChannels),
m_init(false),
m_colorHistogram(NULL)
{
	Initialize(i_numBins);
}

//
// Copy constructor
//
ColorHistExtraction::ColorHistExtraction(const ColorHistExtraction& i_ref)
{
	throw new GeneralException("Exception in ColorHistExtraction::ColorHistExtraction: copy constructor not yet implemented.",__FILE__,__LINE__);
}

//
// BufferedNode constructor
//
ColorHistExtraction::ColorHistExtraction(string nodeName, ParameterSet params)
: VisualFeaturesExtraction<double>(nodeName, params),
m_init(false),
m_colorHistogram(NULL)
{
	m_imageInID = addInput("IN_IMAGE");
	m_numBinsInID = addInput("NUM_BINS");
	m_roiInID = addInput("CURRENT_ROI");
	m_targetInID = addInput("CURRENT_TARGET");
	m_targetDescIdxInID = addInput("TARGET_DESC_IDX");
	m_useNextImgInID = addInput("PREPROCESS_FRAME");
	
	m_featuresOutID = addOutput("OUT_FEATURES");
	m_msLocOutID = addOutput("OUT_MS_LOCATION");
	m_ppCompletedOutID = addOutput("PREPROCESS_COMPLETED");

	m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
	m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
	m_numChannels = dereference_cast<int>(parameters.get("NUM_CHANNELS"));
}
	
ColorHistExtraction::~ColorHistExtraction()
{
	delete m_colorHistogram;
	
	cvReleaseImage(&m_curImage);
}

// Modified BufferedNode request method to support cyclic node connection
void ColorHistExtraction::request(int output_id, const ParameterSet &req) 
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
	
	// Every output usually requires these inputs
	ParameterSet nextImgReq, numBinsReq;
	nextImgReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_useNextImgInID].lookAhead+outputLookAhead)));
	nextImgReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_useNextImgInID].lookBack+outputLookBack)));
	inputs[m_useNextImgInID].node->request(inputs[m_useNextImgInID].outputID,nextImgReq);
	
	numBinsReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_numBinsInID].lookAhead+outputLookAhead)));
	numBinsReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_numBinsInID].lookBack+outputLookBack)));
	inputs[m_numBinsInID].node->request(inputs[m_numBinsInID].outputID,numBinsReq);
	
	if (output_id == m_featuresOutID) {
		ParameterSet myReq;
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookBack+outputLookBack)));
		inputs[m_roiInID].node->request(inputs[m_roiInID].outputID,myReq);
	}
	else if (output_id == m_msLocOutID) {
		ParameterSet myReq, myReq2;
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_targetDescIdxInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_targetDescIdxInID].lookBack+outputLookBack)));
		inputs[m_targetDescIdxInID].node->request(inputs[m_targetDescIdxInID].outputID,myReq);
		
		myReq2.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookAhead+outputLookAhead)));
		myReq2.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_targetInID].lookBack+outputLookBack)));
		inputs[m_targetInID].node->request(inputs[m_targetInID].outputID,myReq2);
	}
	else if (output_id == m_ppCompletedOutID) {
		ParameterSet myReq;
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookBack+outputLookBack)));
		inputs[m_imageInID].node->request(inputs[m_imageInID].outputID, myReq);
	}
	else {
		throw new GeneralException ("ColorHistExtraction::request : unknown output ID.",__FILE__,__LINE__);
	}

}

void ColorHistExtraction::calculate(int output_id, int count, Buffer &out)
{
	try {
		if (!m_init) {
			RCPtr<Vector<int> > numBinsRef = getInput(m_numBinsInID, count);
			Initialize(&(*numBinsRef));
		}
		
		bool useNext = dereference_cast<bool>(getInput(m_useNextImgInID, count));
		
		if (useNext) {
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
		
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("ColorHistExtraction::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Copy input image
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
		}
		
		if (output_id == m_featuresOutID) {
			ObjectRef roiRef = getInput(m_roiInID, count);
			
			if (!roiRef->isNil()) {
				ExtractFeatures(&(object_cast<VisualROI>(roiRef)));
				
				/*
				VisualHistogramDesc<double, unsigned char> *outFeatures = 
					new VisualHistogramDesc<double, unsigned char>(*m_colorHistogram);
				*/
				Vector<VisualFeatureDesc<double> *> *outVec = Vector<VisualFeatureDesc<double> *>::alloc(1);
				(*outVec)[0] = new VisualHistogramDesc<double, unsigned char>(*m_colorHistogram);
				
				(*outputs[m_featuresOutID].buffer)[count] = ObjectRef(outVec);
			}
			else {
				(*outputs[m_featuresOutID].buffer)[count] = ObjectRef(nilObject);
			}
		}
		else if (output_id == m_msLocOutID) {
			// X,Y location
			Vector<double> *outMSLoc = new Vector<double>(2);
			
			ObjectRef targetRef = getInput(m_targetInID, count);
			
			if (!targetRef->isNil()) {
				// Get descriptor index to use
				int descIdx = dereference_cast<int>(getInput(m_targetDescIdxInID, count));
				
				EstimateMSLocation(&(object_cast<VisualTarget<double> >(targetRef)), descIdx, outMSLoc);
				
				(*outputs[m_msLocOutID].buffer)[count] = ObjectRef(outMSLoc);
			}
			else {
				(*outputs[m_msLocOutID].buffer)[count] = ObjectRef(nilObject);
			}
		}
		else if (output_id == m_ppCompletedOutID) {
			// Preprocess image than output true
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
		
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("ColorHistExtraction::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Copy input image
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
			(*outputs[m_ppCompletedOutID].buffer)[count] = ObjectRef(Int::alloc(1));
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in ColorHistExtraction::calculate:",__FILE__,__LINE__));
	}
}

void ColorHistExtraction::ExtractFeatures(VisualROI *i_roi)
{
	try {
		m_colorHistogram->ComputeKernelWeightedHist((const unsigned char *)m_curImage->imageData, 
			255, 1.0, m_width, m_height, i_roi);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in ColorHistExtraction::ExtractFeatures:",__FILE__,__LINE__));
	}
}

void ColorHistExtraction::ExtractFeatures(IplImage *i_input, VisualROI *i_roi)
{
	try {
		m_colorHistogram->ComputeKernelWeightedHist((const unsigned char *)i_input->imageData, 
			255, 1.0, m_width, m_height, i_roi);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in ColorHistExtraction::ExtractFeatures:",__FILE__,__LINE__));
	}
}

void ColorHistExtraction::EstimateMSLocation(const VisualTarget<double> *i_targetRef, 
	int i_descIdx, Vector<double> *o_msLocVec)
{
	try {
		const double *refModelBins = i_targetRef->GetCstDescriptor(i_descIdx)->GetCstFeatures();
		const VisualROI *curROI = i_targetRef->GetCstROI();
		double cueWeight = i_targetRef->GetCueWeight(i_descIdx);
		
		// Extract candidate features histogram
		m_colorHistogram->ComputeKernelWeightedHist((const unsigned char *)m_curImage->imageData, 
			255, 1.0, m_width, m_height, curROI);
		
		// Evaluate mean shift location
		m_colorHistogram->ComputeMSLocation((const unsigned char *)m_curImage->imageData, 
			255, 1.0, m_width, m_height, curROI, refModelBins, cueWeight, o_msLocVec);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in ColorHistExtraction::EstimateMSLocation:",__FILE__,__LINE__));
	}
}

//
// Private mehtods
//

void ColorHistExtraction::Initialize(const Vector<int> *i_numBins)
{
	m_numPixels = m_width*m_height;
	m_numBytesInFrame = m_numPixels*3;
	
	CvSize imgSize;
	imgSize.width = m_width;
	imgSize.height = m_height;
	m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
	
	m_colorHistogram = new VisualHistogramDesc<double, unsigned char>(e_VISUALHIST_BhattacharyyaCoeff, 
		true, m_numChannels, i_numBins);
	
	m_init = true;
}

}//namespace RobotFlow
