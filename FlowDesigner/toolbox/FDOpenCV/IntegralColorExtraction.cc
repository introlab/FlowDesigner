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
#include "IntegralColorExtraction.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

DECLARE_NODE(IntegralColorExtraction)
DECLARE_TYPE(IntegralColorExtraction)

  /*Node
   *
   * @name IntegralColorExtraction
   * @category RobotFlow:Vision:FeaturesExtraction
   * @description Integral color features extraction.
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
   * @parameter_name NUM_HORI_RECT
   * @parameter_type int
   * @parameter_value 2
   * @parameter_description Number of horizontal rectangle to use for integral features.
   *
   * @parameter_name NUM_VERT_RECT
   * @parameter_type int
   * @parameter_value 2
   * @parameter_description Number of vertical rectangle to use for integral features.
   *
   * @parameter_name USE_RECT_DIFF
   * @parameter_type bool
   * @parameter_value true
   * @parameter_description Features are differences between rectangle regions.
   *
   * @parameter_name USE_BOUNDARY_DIFF
   * @parameter_type bool
   * @parameter_value true
   * @parameter_description Flag indicating to detect a color boundary for a better scale adaptation.
   *
   * @parameter_name BOUNDARY_DIFF_THRESH
   * @parameter_type float
   * @parameter_value 25.0
   * @parameter_description Minimum difference between boundary and ROI rectangle mean values.
   *
   * @input_name IN_IMAGE
   * @input_type Image
   * @input_description Current frame to process.
   *
   * @input_name CURRENT_ROI
   * @input_type VisualROI
   * @input_description The current region of interest.
   *
   * @input_name PREPROCESS_FRAME
   * @input_type bool
   * @input_description Flag indicating to preprocess a new image.
   *
   * @output_name OUT_FEATURES
   * @output_type Vector<VisualFeatureDesc<double> *>
   * @output_description Output features descriptor.
   *
   * @output_name PREPROCESS_COMPLETED
   * @output_type int
   * @output_description Output to force preprocessing.
   *
   END*/

   
//
// Default constructor for Object 
//
IntegralColorExtraction::IntegralColorExtraction()
: m_width(-1),
m_height(-1),
m_numChannels(-1),
m_numHoriIntRect(0),
m_numVertIntRect(0),
m_maxValue(0),
m_useRectDiff(false),
m_useBoundary(false),
m_boundaryMeanDiffThresh(0.0),
m_tmpMeanFeatures(NULL),
m_curMeanVal(NULL),
m_featVect(NULL)
{

}

//
// Constructor with complete intialisation
//
IntegralColorExtraction::IntegralColorExtraction(int i_width, int i_height, 
	int i_numChannels, int i_numHoriIntRect, int i_numVertIntRect,
	double i_maxValue, bool i_useRectDiff, bool i_useBoundary, 
	double i_boundaryMeanDiffThresh)
: VisualFeaturesExtraction<double>(string("IntegralColorExtraction"), ParameterSet()),
m_width(i_width),
m_height(i_height),
m_numChannels(i_numChannels),
m_numHoriIntRect(i_numHoriIntRect),
m_numVertIntRect(i_numVertIntRect),
m_maxValue(i_maxValue),
m_useRectDiff(i_useRectDiff),
m_useBoundary(i_useBoundary),
m_boundaryMeanDiffThresh(i_boundaryMeanDiffThresh),
m_tmpMeanFeatures(NULL),
m_curMeanVal(NULL),
m_featVect(NULL)
{
	Initialize();
}

//
// BufferedNode constructor
//
IntegralColorExtraction::IntegralColorExtraction(string nodeName, ParameterSet params)
: VisualFeaturesExtraction<double>(nodeName, params),
m_tmpMeanFeatures(NULL),
m_curMeanVal(NULL),
m_featVect(NULL)
{
	m_imageInID = addInput("IN_IMAGE");
	m_roiInID = addInput("CURRENT_ROI");
	m_useNextImgInID = addInput("PREPROCESS_FRAME");
	
	m_featuresOutID = addOutput("OUT_FEATURES");
	m_ppCompletedOutID = addOutput("PREPROCESS_COMPLETED");

	m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
	m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
	m_numChannels = dereference_cast<int>(parameters.get("NUM_CHANNELS"));
	m_numHoriIntRect = dereference_cast<int>(parameters.get("NUM_HORI_RECT"));
	m_numVertIntRect = dereference_cast<int>(parameters.get("NUM_VERT_RECT"));
	m_useRectDiff = dereference_cast<bool>(parameters.get("USE_RECT_DIFF"));
	m_useBoundary = dereference_cast<bool>(parameters.get("USE_BOUNDARY_DIFF"));
	m_boundaryMeanDiffThresh = dereference_cast<float>(parameters.get("BOUNDARY_DIFF_THRESH"));
	
	if (m_useRectDiff) {
		m_maxValue = 510.0;
	}
	else {
		m_maxValue = 255.0;
	}
	
	Initialize();
}
	
IntegralColorExtraction::~IntegralColorExtraction()
{
	for (int c=0; c<m_numChannels; c++) {
		cvReleaseImage(&(m_sumImage[c]));
		cvReleaseImage(&(m_chImage[c]));
	}
	cvReleaseImage(&m_curImage);
	
	delete [] m_sumPixPtr;
	delete [] m_chPixPtr;
	delete [] m_sumImage;
	delete [] m_chImage;
	
	delete [] m_curMeanVal;
	delete [] m_tmpMeanFeatures;
}

// Modified BufferedNode request method to support cyclic node connection
void IntegralColorExtraction::request(int output_id, const ParameterSet &req) 
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
	ParameterSet nextImgReq;
	nextImgReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_useNextImgInID].lookAhead+outputLookAhead)));
	nextImgReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_useNextImgInID].lookBack+outputLookBack)));
	inputs[m_useNextImgInID].node->request(inputs[m_useNextImgInID].outputID,nextImgReq);
	
	if (output_id == m_featuresOutID) {
		ParameterSet myReq;
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_roiInID].lookBack+outputLookBack)));
		inputs[m_roiInID].node->request(inputs[m_roiInID].outputID,myReq);
	}
	else if (output_id == m_ppCompletedOutID) {
		ParameterSet myReq;
		myReq.add("LOOKAHEAD", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookAhead+outputLookAhead)));
		myReq.add("LOOKBACK", ObjectRef(Int::alloc(inputsCache[m_imageInID].lookBack+outputLookBack)));
		inputs[m_imageInID].node->request(inputs[m_imageInID].outputID, myReq);
	}
	else {
		throw new GeneralException ("IntegralColorExtraction::request : unknown output ID.",__FILE__,__LINE__);
	}

}

void IntegralColorExtraction::calculate(int output_id, int count, Buffer &out)
{
	try {
		bool useNext = dereference_cast<bool>(getInput(m_useNextImgInID, count));
		
		if (useNext) {
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
		
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("IntegralColorExtraction::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Preprocess input image
			Preprocess((const unsigned char *)(imageRef->get_data()));
		}
		
		if (output_id == m_featuresOutID) {
			ObjectRef roiRef = getInput(m_roiInID, count);
			
			if (!roiRef->isNil()) {
				RCPtr<VisualROI> roiPtr = roiRef;
				ExtractFeatures(roiPtr.get());
				
				(*outputs[m_featuresOutID].buffer)[count] = m_featVect;
			}
			else {
				(*outputs[m_featuresOutID].buffer)[count] = ObjectRef(nilObject);
			}
		}
		else if (output_id == m_ppCompletedOutID) {
			// Preprocess image than output true
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
		
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("IntegralColorExtraction::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Preprocess input image
			Preprocess((const unsigned char *)(imageRef->get_data()));
			
			(*outputs[m_ppCompletedOutID].buffer)[count] = ObjectRef(Int::alloc(1));
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in IntegralColorExtraction::calculate:",__FILE__,__LINE__));
	}
}

void IntegralColorExtraction::ExtractFeatures(VisualROI *i_roi)
{
	try {
		CvPoint ulcLimit, lrcLimit, curULC, curLRC;
		int roiWidth, roiHeight, deltaX, deltaY;
		double numPixelsInSubROI;
		int c,i,j;
		double *featPtr = (*m_featVect)[0]->GetFeatures();
		
		int cenX = i_roi->GetXCen();
		int cenY = i_roi->GetYCen();
		if (cenX < 0 || cenX >= m_width ||
			cenY < 0 || cenY >= m_height) {
			// ROI center is out of image plane
			// Ignore current ROI
			(*m_featVect)[0]->SetValidity(false);
			return;
		}
		
		ulcLimit.x = cenX - i_roi->GetHSX();
		if (ulcLimit.x < 0) {
			ulcLimit.x = 0;
		}
		else if (ulcLimit.x >= m_width) {
			ulcLimit.x = m_width-1;
		}
		
		ulcLimit.y = cenY - i_roi->GetHSY();
		if (ulcLimit.y < 0) {
			ulcLimit.y = 0;
		}
		else if (ulcLimit.y >= m_height) {
			ulcLimit.y = m_height-1;
		}
		
		lrcLimit.x = cenX + i_roi->GetHSX();
		if (lrcLimit.x < 0) {
			lrcLimit.x = 0;
		}
		else if (lrcLimit.x >= m_width) {
			lrcLimit.x = m_width-1;
		}
		
		lrcLimit.y = cenY + i_roi->GetHSY();
		if (lrcLimit.y < 0) {
			lrcLimit.y = 0;
		}
		else if (lrcLimit.y >= m_height) {
			lrcLimit.y = m_height-1;
		}
		
		roiWidth = lrcLimit.x - ulcLimit.x;
		roiHeight = lrcLimit.y - ulcLimit.y;
		
		if (roiWidth==0 || roiHeight==0) {
			// Invalid ROI
			(*m_featVect)[0]->SetValidity(false);
			return;
		}
		else {
			(*m_featVect)[0]->SetValidity(true);
		}
		
		//cout << "ROI Info: ulc=(" << ulcLimit.x << "," << ulcLimit.y << ") lrc=(" << lrcLimit.x << "," << lrcLimit.y << endl;
		
		if (m_numHoriIntRect) {
			deltaX = roiWidth/m_numHoriIntRect;
		}
		else {
			deltaX = roiWidth;
		}
		
		if (m_numVertIntRect) {
			deltaY = roiHeight/m_numVertIntRect;
		}
		else {
			deltaY = roiHeight;
		}
		
		for (c=0; c<m_numChannels; c++) {
			m_sumPixPtr[c] = (int *)(m_sumImage[c]->imageData);
		}
		numPixelsInSubROI = deltaX*deltaY;
		
		if (numPixelsInSubROI == 0) {
			// Invalid ROI
			(*m_featVect)[0]->SetValidity(false);
			return;
		}
		
		curULC.y = ulcLimit.y;
		curLRC.y = curULC.y + deltaY;
		
		if (m_useRectDiff) {
			double *p_tmpMeanFeatures = m_tmpMeanFeatures;
			
			// First pass: compute mean values
			for (i=0; i<m_numVertIntRect; i++) {
				curULC.x = ulcLimit.x;
				curLRC.x = curULC.x + deltaX;
	
				for (j=0; j<m_numHoriIntRect; j++) {
					for (c=0; c<m_numChannels; c++) {
						// Compute mean color channel
						*p_tmpMeanFeatures = (double)(m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curLRC.x] - 
							m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curULC.x] - 
							m_sumPixPtr[c][curULC.y*m_imgSumWidth+curLRC.x] + 
							m_sumPixPtr[c][curULC.y*m_imgSumWidth+curULC.x])/numPixelsInSubROI;
						
						*featPtr++ = *p_tmpMeanFeatures++;
					}
					
					curULC.x += deltaX;
					curLRC.x += deltaX;
				}
				
				curULC.y += deltaY;
				curLRC.y += deltaY;
			}
			
			// Second pass: compute differences
			for (i=0; i<m_numIntRect; i++) {
				for (c=0; c<m_numChannels; c++) {
					m_curMeanVal[c] = m_tmpMeanFeatures[i*m_numChannels + c];
				}
				
				for (j=0; j<m_numIntRect; j++) {
					if (j != i) {
						for (c=0; c<m_numChannels; c++) {
							*featPtr++ = m_tmpMeanFeatures[j*m_numChannels + c] - m_curMeanVal[c] + 255.0;
						}
					}
				}
			}
		}
		else {
			for (i=0; i<m_numVertIntRect; i++) {
				curULC.x = ulcLimit.x;
				curLRC.x = curULC.x + deltaX;
	
				for (j=0; j<m_numHoriIntRect; j++) {
					for (c=0; c<m_numChannels; c++) {
						// Compute mean color channel
						double tmpVal = (double)(m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curLRC.x] - 
							m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curULC.x] - 
							m_sumPixPtr[c][curULC.y*m_imgSumWidth+curLRC.x] + 
							m_sumPixPtr[c][curULC.y*m_imgSumWidth+curULC.x])/numPixelsInSubROI;
						
						*featPtr++ = tmpVal;
					}
					
					curULC.x += deltaX;
					curLRC.x += deltaX;
				}
				
				curULC.y += deltaY;
				curLRC.y += deltaY;
			}
		}
		
		if (m_useBoundary) {
			CvPoint boundULC, boundLRC;
			double numPixelsInBoundary;
			int boundWidth = cvRound((float)(roiWidth)*0.1f);
			int boundHeight = cvRound((float)(roiHeight)*0.1f);
			
			// Check top boundary
			if (ulcLimit.y >= boundHeight) {
				int numDiff = 0;
				boundULC.y = ulcLimit.y - boundHeight;
				boundLRC.y = ulcLimit.y;
				boundULC.x = ulcLimit.x;
				boundLRC.x = lrcLimit.x;
				
				curULC.y = ulcLimit.y;
				curLRC.y = ulcLimit.y + deltaY;
				curULC.x = ulcLimit.x;
				curLRC.x = lrcLimit.x;
				
				numPixelsInBoundary = roiWidth*boundHeight;
				numPixelsInSubROI = roiWidth*deltaY;
				
				for (c=0; c<m_numChannels; c++) {
					double boundVal = (double)(m_sumPixPtr[c][boundLRC.y*m_imgSumWidth+boundLRC.x] - 
						m_sumPixPtr[c][boundLRC.y*m_imgSumWidth+boundULC.x] - 
						m_sumPixPtr[c][boundULC.y*m_imgSumWidth+boundLRC.x] + 
						m_sumPixPtr[c][boundULC.y*m_imgSumWidth+boundULC.x])/numPixelsInBoundary;
					double roiVal = (double)(m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curLRC.x] - 
						m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curULC.x] - 
						m_sumPixPtr[c][curULC.y*m_imgSumWidth+curLRC.x] + 
						m_sumPixPtr[c][curULC.y*m_imgSumWidth+curULC.x])/numPixelsInSubROI;
					
					//cout << "Top boundary: roiVal=" << roiVal << " boundVal=" << boundVal << endl;
					
					if (abs(boundVal-roiVal) > m_boundaryMeanDiffThresh) {
						numDiff++;
					}
				}
				
				if (numDiff > 0) {
					*featPtr++ = 2.0;
				}
				else {
					*featPtr++ = 0.0;
				}
			}
			else {
				// Boundary out of image
				// Mark as unknown boundary
				*featPtr++ = 1.0;
			}
			
			// Check bottom boundary
			if (lrcLimit.y+boundHeight < m_height) {
				int numDiff = 0;
				boundULC.y = lrcLimit.y;
				boundLRC.y = lrcLimit.y + boundHeight;
				boundULC.x = ulcLimit.x;
				boundLRC.x = lrcLimit.x;
				
				curULC.y = lrcLimit.y - deltaY;
				curLRC.y = lrcLimit.y;
				curULC.x = ulcLimit.x;
				curLRC.x = lrcLimit.x;
				
				numPixelsInBoundary = roiWidth*boundHeight;
				numPixelsInSubROI = roiWidth*deltaY;
				
				for (c=0; c<m_numChannels; c++) {
					double boundVal = (double)(m_sumPixPtr[c][boundLRC.y*m_imgSumWidth+boundLRC.x] - 
						m_sumPixPtr[c][boundLRC.y*m_imgSumWidth+boundULC.x] - 
						m_sumPixPtr[c][boundULC.y*m_imgSumWidth+boundLRC.x] + 
						m_sumPixPtr[c][boundULC.y*m_imgSumWidth+boundULC.x])/numPixelsInBoundary;
					double roiVal = (double)(m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curLRC.x] - 
						m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curULC.x] - 
						m_sumPixPtr[c][curULC.y*m_imgSumWidth+curLRC.x] + 
						m_sumPixPtr[c][curULC.y*m_imgSumWidth+curULC.x])/numPixelsInSubROI;
					
					//cout << "Bottom boundary: roiVal=" << roiVal << " boundVal=" << boundVal << endl;
					
					if (abs(boundVal-roiVal) > m_boundaryMeanDiffThresh) {
						numDiff++;
					}
				}
				
				if (numDiff > 0) {
					*featPtr++ = 2.0;
				}
				else {
					*featPtr++ = 0.0;
				}
			}
			else {
				// Boundary out of image
				// Mark as unknown boundary
				*featPtr++ = 1.0;
			}
			
			// Check left boundary
			if (ulcLimit.x >= boundWidth) {
				int numDiff = 0;
				boundULC.y = ulcLimit.y;
				boundLRC.y = lrcLimit.y;
				boundULC.x = ulcLimit.x - boundWidth;
				boundLRC.x = ulcLimit.x;
				
				curULC.y = ulcLimit.y;
				curLRC.y = lrcLimit.y;
				curULC.x = ulcLimit.x;
				curLRC.x = ulcLimit.x + deltaX;
				
				numPixelsInBoundary = roiHeight*boundWidth;
				numPixelsInSubROI = roiHeight*deltaX;
				
				for (c=0; c<m_numChannels; c++) {
					double boundVal = (double)(m_sumPixPtr[c][boundLRC.y*m_imgSumWidth+boundLRC.x] - 
						m_sumPixPtr[c][boundLRC.y*m_imgSumWidth+boundULC.x] - 
						m_sumPixPtr[c][boundULC.y*m_imgSumWidth+boundLRC.x] + 
						m_sumPixPtr[c][boundULC.y*m_imgSumWidth+boundULC.x])/numPixelsInBoundary;
					double roiVal = (double)(m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curLRC.x] - 
						m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curULC.x] - 
						m_sumPixPtr[c][curULC.y*m_imgSumWidth+curLRC.x] + 
						m_sumPixPtr[c][curULC.y*m_imgSumWidth+curULC.x])/numPixelsInSubROI;
					
					//cout << "Left boundary: roiVal=" << roiVal << " boundVal=" << boundVal << endl;
					
					if (abs(boundVal-roiVal) > m_boundaryMeanDiffThresh) {
						numDiff++;
					}
				}
				
				if (numDiff > 0) {
					*featPtr++ = 2.0;
				}
				else {
					*featPtr++ = 0.0;
				}
			}
			else {
				// Boundary out of image
				// Mark as unknown boundary
				*featPtr++ = 1.0;
			}
			
			// Check right boundary
			if (lrcLimit.x+boundWidth < m_width) {
				int numDiff = 0;
				boundULC.y = ulcLimit.y;
				boundLRC.y = lrcLimit.y;
				boundULC.x = lrcLimit.x;
				boundLRC.x = lrcLimit.x + boundWidth;
				
				curULC.y = ulcLimit.y;
				curLRC.y = lrcLimit.y;
				curULC.x = lrcLimit.x - deltaX;
				curLRC.x = lrcLimit.x;
				
				numPixelsInBoundary = roiHeight*boundWidth;
				numPixelsInSubROI = roiHeight*deltaX;
				
				for (c=0; c<m_numChannels; c++) {
					double boundVal = (double)(m_sumPixPtr[c][boundLRC.y*m_imgSumWidth+boundLRC.x] - 
						m_sumPixPtr[c][boundLRC.y*m_imgSumWidth+boundULC.x] - 
						m_sumPixPtr[c][boundULC.y*m_imgSumWidth+boundLRC.x] + 
						m_sumPixPtr[c][boundULC.y*m_imgSumWidth+boundULC.x])/numPixelsInBoundary;
					double roiVal = (double)(m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curLRC.x] - 
						m_sumPixPtr[c][curLRC.y*m_imgSumWidth+curULC.x] - 
						m_sumPixPtr[c][curULC.y*m_imgSumWidth+curLRC.x] + 
						m_sumPixPtr[c][curULC.y*m_imgSumWidth+curULC.x])/numPixelsInSubROI;
					
					//cout << "Right boundary: roiVal=" << roiVal << " boundVal=" << boundVal << endl;
					
					if (abs(boundVal-roiVal) > m_boundaryMeanDiffThresh) {
						numDiff++;
					}
				}
				
				if (numDiff > 0) {
					*featPtr++ = 2.0;
				}
				else {
					*featPtr++ = 0.0;
				}
			}
			else {
				// Boundary out of image
				// Mark as unknown boundary
				*featPtr++ = 1.0;
			}
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in IntegralColorExtraction::ExtractFeatures:",__FILE__,__LINE__));
	}
}

void IntegralColorExtraction::ExtractFeatures(IplImage *i_input, VisualROI *i_roi)
{
	try {
		Preprocess((const unsigned char *)(i_input->imageData));
		
		ExtractFeatures(i_roi);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in IntegralColorExtraction::ExtractFeatures:",__FILE__,__LINE__));
	}
}

void IntegralColorExtraction::Preprocess(const unsigned char *i_src)
{
	int c, p;
	const unsigned char *p_srcPix = i_src;
	
	// First extract all channels of image
	for (c=0; c<m_numChannels; c++) {
		m_chPixPtr[c] = (unsigned char *)(m_chImage[c]->imageData);
	}
	
	for (p=0; p<m_numPixels; p++) {
		for (c=0; c<m_numChannels; c++) {
			*(m_chPixPtr[c])++ = *p_srcPix++;
		}
	}
	
	// Preprocessing consists of computing the sum of pixels
	// in order to have the integral images.
	for (c=0; c<m_numChannels; c++) {
		cvIntegral(m_chImage[c], m_sumImage[c], NULL, NULL);
	}
}

void IntegralColorExtraction::Preprocess(IplImage *i_srcImg)
{
	int c, p;
	const unsigned char *p_srcPix = (const unsigned char *)i_srcImg->imageData;
	
	// First extract all channels of image
	for (c=0; c<m_numChannels; c++) {
		m_chPixPtr[c] = (unsigned char *)(m_chImage[c]->imageData);
	}
	
	for (p=0; p<m_numPixels; p++) {
		for (c=0; c<m_numChannels; c++) {
			*(m_chPixPtr[c])++ = *p_srcPix++;
		}
	}
	
	// Preprocessing consists of computing the sum of pixels
	// in order to have the integral images.
	for (c=0; c<m_numChannels; c++) {
		cvIntegral(m_chImage[c], m_sumImage[c], NULL, NULL);
	}
}

//
// Private methods
//

void IntegralColorExtraction::Initialize()
{
	m_numPixels = m_width*m_height;
	m_numBytesInFrame = m_numPixels*m_numChannels;
	m_numIntRect = m_numHoriIntRect*m_numVertIntRect;
	
	int numIntRectFeatures;
	if (m_useRectDiff) {
		numIntRectFeatures = m_numIntRect+m_numIntRect*(m_numIntRect-1);
		m_tmpMeanFeatures = new double[m_numIntRect*m_numChannels];
		m_curMeanVal = new double[m_numChannels];
	}
	else {
		numIntRectFeatures = m_numIntRect;
	}
	
	m_featVect = RCPtr<Vector<VisualFeatureDesc<double> *> >(Vector<VisualFeatureDesc<double> *>::alloc(1));
	(*m_featVect)[0] = new VisualIntegralDesc<double>(
		e_VISUALINTDESC_EuclideanDist, 
		m_numChannels, 
		numIntRectFeatures, 
		m_maxValue, 
		m_useRectDiff,
		m_useBoundary);
	
	m_chImage = new IplImage *[m_numChannels]; 
	m_sumImage = new IplImage *[m_numChannels];
	m_chPixPtr = new unsigned char*[m_numChannels];
	m_sumPixPtr = new int *[m_numChannels];
	
	CvSize imgSize;
	imgSize.width = m_width;
	imgSize.height = m_height;
	m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
	
	// Sum images are of size (m_width+1)x(m_height+1)
	m_imgSumWidth = m_width+1;
	CvSize sumSize;
	sumSize.width = m_imgSumWidth;
	sumSize.height = m_height+1;
	
	for (int c=0; c<m_numChannels; c++) {
		m_chImage[c] = cvCreateImage(imgSize, IPL_DEPTH_8U, 1);
		m_sumImage[c] = cvCreateImage(sumSize, IPL_DEPTH_32S, 1);
	}
}

}//namespace RobotFlow
