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
#include "IntegralEdgesOriExtraction.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

DECLARE_NODE(IntegralEdgesOriExtraction)
DECLARE_TYPE(IntegralEdgesOriExtraction)

  /*Node
   *
   * @name IntegralEdgesOriExtraction
   * @category RobotFlow:Vision:FeaturesExtraction
   * @description Integral edges orientation features extraction.
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
   * @parameter_name NUM_ORIENTATIONS
   * @parameter_type int
   * @parameter_value 4
   * @parameter_description Number of edge orientations.
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
   * @parameter_name MIN_EDGES_STRENGTH
   * @parameter_type float
   * @parameter_value 100.0
   * @parameter_description Threshold to remove noisy/weak edges.
   *
   * @parameter_name MAX_EDGES_STRENGTH
   * @parameter_type float
   * @parameter_value 1000.0
   * @parameter_description Limit on the edge strength.
   *
   * @parameter_name USE_RECT_DIFF
   * @parameter_type bool
   * @parameter_value true
   * @parameter_description Features are differences between rectangle regions.
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


static const double k_IEOE_pi = 3.14159265358979323846;
static const double k_IEOE_2piInv = 1.0/(2.0*k_IEOE_pi);
   
//
// Default constructor for Object 
//
IntegralEdgesOriExtraction::IntegralEdgesOriExtraction()
: m_width(-1),
m_height(-1),
m_numChannels(-1),
m_numOriBins(-1),
m_edgesStrTresh(0.0),
m_maxStrengthValue(0.0),
m_useRectDiff(false),
m_tmpMeanFeatures(NULL),
m_curMeanVal(NULL),
m_featVect(NULL)
{

}

//
// Constructor with complete intialisation
//
IntegralEdgesOriExtraction::IntegralEdgesOriExtraction(int i_width, int i_height, 
	int i_numChannels, int i_numHoriIntRect, int i_numVertIntRect,
	int i_numOriBins, double i_edgesStrTresh,
	double i_maxStrengthValue, bool i_useRectDiff)
: VisualFeaturesExtraction<double>(string("IntegralEdgesOriExtraction"), ParameterSet()),
m_width(i_width),
m_height(i_height),
m_numChannels(i_numChannels),
m_numHoriIntRect(i_numHoriIntRect),
m_numVertIntRect(i_numVertIntRect),
m_numOriBins(i_numOriBins),
m_edgesStrTresh(i_edgesStrTresh),
m_maxStrengthValue(i_maxStrengthValue),
m_useRectDiff(i_useRectDiff),
m_tmpMeanFeatures(NULL),
m_curMeanVal(NULL),
m_featVect(NULL)
{
	Initialize();
}

//
// BufferedNode constructor
//
IntegralEdgesOriExtraction::IntegralEdgesOriExtraction(string nodeName, ParameterSet params)
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
	m_numOriBins = dereference_cast<int>(parameters.get("NUM_ORIENTATIONS"));
	m_numHoriIntRect = dereference_cast<int>(parameters.get("NUM_HORI_RECT"));
	m_numVertIntRect = dereference_cast<int>(parameters.get("NUM_VERT_RECT"));
	m_edgesStrTresh = dereference_cast<float>(parameters.get("MIN_EDGES_STRENGTH"));
	m_maxStrengthValue = dereference_cast<float>(parameters.get("MAX_EDGES_STRENGTH"));
	m_useRectDiff = dereference_cast<bool>(parameters.get("USE_RECT_DIFF"));
	
	Initialize();
}
	
IntegralEdgesOriExtraction::~IntegralEdgesOriExtraction()
{
	for (int b=0; b<m_numOriBins; b++) {
		cvReleaseImage(&(m_edgesOriSum[b]));
		cvReleaseImage(&(m_edgesOri[b]));
	}
	
	delete [] m_edgesOriSumPix;
	delete [] m_edgesOriPix;
	delete [] m_edgesOriSum;
	delete [] m_edgesOri;
	
	cvReleaseImage(&m_oriYImage);
	cvReleaseImage(&m_oriXImage);
	cvReleaseImage(&m_grayImage);
	cvReleaseImage(&m_curImage);
	
	delete [] m_curMeanVal;
	delete [] m_tmpMeanFeatures;
}

// Modified BufferedNode request method to support cyclic node connection
void IntegralEdgesOriExtraction::request(int output_id, const ParameterSet &req) 
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
		ParameterSet myReq, myReq2;
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
		throw new GeneralException ("IntegralEdgesOriExtraction::request : unknown output ID.",__FILE__,__LINE__);
	}

}

void IntegralEdgesOriExtraction::calculate(int output_id, int count, Buffer &out)
{
	try {
		bool useNext = dereference_cast<bool>(getInput(m_useNextImgInID, count));
		
		if (useNext) {
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
		
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("IntegralEdgesOriExtraction::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Copy input image
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
			
			// Preprocess input image
			Preprocess(m_curImage);
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
				throw new GeneralException ("IntegralEdgesOriExtraction::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Copy input image
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
			
			// Preprocess input image
			Preprocess(m_curImage);
			
			(*outputs[m_ppCompletedOutID].buffer)[count] = ObjectRef(Int::alloc(1));
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in IntegralEdgesOriExtraction::calculate:",__FILE__,__LINE__));
	}
}

void IntegralEdgesOriExtraction::ExtractFeatures(VisualROI *i_roi)
{
	try {
		CvPoint ulcLimit, lrcLimit, curULC, curLRC;
		int roiWidth, roiHeight, deltaX, deltaY;
		double numPixelsInSubROI;
		int b,i,j;
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
					for (b=0; b<m_numOriBins; b++) {
						// Compute mean color channel
						*p_tmpMeanFeatures = (m_edgesOriSumPix[b][lrcLimit.y*m_imgSumWidth+lrcLimit.x] - 
							m_edgesOriSumPix[b][lrcLimit.y*m_imgSumWidth+ulcLimit.x] - 
							m_edgesOriSumPix[b][ulcLimit.y*m_imgSumWidth+lrcLimit.x] + 
							m_edgesOriSumPix[b][ulcLimit.y*m_imgSumWidth+ulcLimit.x])/numPixelsInSubROI;
						
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
				for (b=0; b<m_numOriBins; b++) {
					m_curMeanVal[b] = m_tmpMeanFeatures[i*m_numOriBins + b];
				}
				
				for (j=0; j<m_numIntRect; j++) {
					if (j != i) {
						for (b=0; b<m_numOriBins; b++) {
							*featPtr++ = m_tmpMeanFeatures[j*m_numOriBins + b] - m_curMeanVal[b] + m_maxStrengthValue;
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
					// Compute orientation features
					for (b=0; b<m_numOriBins; b++) {
						*featPtr++ = (m_edgesOriSumPix[b][lrcLimit.y*m_imgSumWidth+lrcLimit.x] - 
							m_edgesOriSumPix[b][lrcLimit.y*m_imgSumWidth+ulcLimit.x] - 
							m_edgesOriSumPix[b][ulcLimit.y*m_imgSumWidth+lrcLimit.x] + 
							m_edgesOriSumPix[b][ulcLimit.y*m_imgSumWidth+ulcLimit.x])/numPixelsInSubROI;
					}
					
					curULC.x += deltaX;
					curLRC.x += deltaX;
				}
				
				curULC.y += deltaY;
				curLRC.y += deltaY;
			}
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in IntegralEdgesOriExtraction::ExtractFeatures:",__FILE__,__LINE__));
	}
}

void IntegralEdgesOriExtraction::ExtractFeatures(IplImage *i_input, VisualROI *i_roi)
{
	try {
		Preprocess(i_input);
		
		ExtractFeatures(i_roi);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in IntegralEdgesOriExtraction::ExtractFeatures:",__FILE__,__LINE__));
	}
}

void IntegralEdgesOriExtraction::Preprocess(IplImage *i_srcImg)
{
	int b, p;
	
	if (!m_edgesOriSumRef->isNil()) {
		FD::RCPtr<FD::Vector<double *> > imgVecRef = m_edgesOriSumRef;
		// Edges orientation integral images are already computed
		for (b=0; b<m_numOriBins; b++) {
			m_edgesOriSumPix[b] = (*imgVecRef)[b];
		}
		
		return;
	}
	
	// Convert to graysacle
	if (m_numChannels == 3) {
		cvCvtColor(i_srcImg, m_grayImage, CV_BGR2GRAY);
	}
	else if (m_numChannels == 1) {
		// Copy input image
		memcpy(m_grayImage->imageData, i_srcImg->imageData, m_numBytesInFrame);
	}
	else {
		throw new GeneralException ("IntegralEdgesOriExtraction::Preprocess : only images with 1 or 3 channels are supported.",__FILE__,__LINE__);
	}
	
	cvSobel(m_grayImage, m_oriXImage, 1, 0, 3);
	cvSobel(m_grayImage, m_oriYImage, 0, 1, 3);
	
	signed short *p_oriXPixels = (signed short *)(m_oriXImage->imageData);
	signed short *p_oriYPixels = (signed short *)(m_oriYImage->imageData);
	// Each orientation is assigned a channel for integral image processing
	for (b=0; b<m_numOriBins; b++) {
		// Assign pointers to each orientation image
		m_edgesOriPix[b] = (float *)(m_edgesOri[b]->imageData);
	}
	for (p=0; p<m_numPixels; p++) {
		// Compute edge strength
		float strength = sqrt((float)((*p_oriXPixels)*(*p_oriXPixels) + (*p_oriYPixels)*(*p_oriYPixels)));
		if (strength < m_edgesStrTresh) {
			strength = 0;
		}
		else if (strength > m_maxStrengthValue) {
			strength = m_maxStrengthValue;
		}
		
		// Compute edge orientation
		// Orientation will be between [0, 2PI]
		float angle = atan2((float)(*p_oriYPixels), (float)(*p_oriXPixels)) + k_IEOE_pi;
		
		// Determine the orientation image channel
		int angleChannel = cvRound((angle*k_IEOE_2piInv)*(float)(m_numOriBins-1));
		if (angleChannel < 0) {
			angleChannel = 0;
		}
		else if (angleChannel >= m_numOriBins) {
			angleChannel = m_numOriBins-1;
		}
		
		// Assign edge strengh to appropriate angle channel image
		for (b=0; b<m_numOriBins; b++) {
			if (b == angleChannel) {
				*(m_edgesOriPix[b])++ = strength;
			}
			else {
				*(m_edgesOriPix[b])++ = 0.f;
			}
		}
		
		p_oriXPixels++;
		p_oriYPixels++;
	}
	
	// Compute integral images on all orientation channels
	for (b=0; b<m_numOriBins; b++) {
		cvIntegral(m_edgesOri[b], m_edgesOriSum[b], NULL, NULL);
	}
	
	// Assign pointers on each orientation integral image
	for (b=0; b<m_numOriBins; b++) {
		m_edgesOriSumPix[b] = (double *)(m_edgesOriSum[b]->imageData);
	}
}

//
// Private methods
//

void IntegralEdgesOriExtraction::Initialize()
{
	m_numPixels = m_width*m_height;
	m_numBytesInFrame = m_numPixels*m_numChannels;
	m_numIntRect = m_numHoriIntRect*m_numVertIntRect;
	
	int numIntRectFeatures;
	if (m_useRectDiff) {
		m_maxFeatValue = 2.0*m_maxStrengthValue;
		numIntRectFeatures = m_numIntRect + m_numIntRect*(m_numIntRect-1);
		m_tmpMeanFeatures = new double[m_numIntRect*m_numOriBins];
		m_curMeanVal = new double[m_numOriBins];
	}
	else {
		m_maxFeatValue = m_maxStrengthValue;
		numIntRectFeatures = m_numIntRect;
	}
	
	m_featVect = RCPtr<Vector<VisualFeatureDesc<double> *> >(Vector<VisualFeatureDesc<double> *>::alloc(1));
	(*m_featVect)[0] = new VisualIntegralDesc<double>(
		e_VISUALINTDESC_EuclideanDist, 
		m_numOriBins, 
		numIntRectFeatures, 
		m_maxFeatValue, 
		m_useRectDiff,
		false);
	
	CvSize imgSize;
	imgSize.width = m_width;
	imgSize.height = m_height;
	m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
	m_grayImage = cvCreateImage( imgSize, IPL_DEPTH_8U, 1 );
	m_oriXImage = cvCreateImage( imgSize, IPL_DEPTH_16S, 1 );
	m_oriYImage = cvCreateImage( imgSize, IPL_DEPTH_16S, 1 );
	
	CvSize sumImgSize;
	m_imgSumWidth = m_width+1;
	sumImgSize.width = m_imgSumWidth;
	sumImgSize.height = m_height+1;
	
	m_edgesOri = new IplImage*[m_numOriBins];
	m_edgesOriSum = new IplImage*[m_numOriBins];
	m_edgesOriPix = new float*[m_numOriBins];
	m_edgesOriSumPix = new double*[m_numOriBins];
	
	for (int b=0; b<m_numOriBins; b++) {
		m_edgesOri[b] = cvCreateImage( imgSize, IPL_DEPTH_32F, 1 );
		m_edgesOriSum[b] = cvCreateImage( sumImgSize, IPL_DEPTH_64F, 1 );
	}
}

}//namespace RobotFlow
