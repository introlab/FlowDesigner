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
#include "IntegralLBPExtraction.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

DECLARE_NODE(IntegralLBPExtraction)
DECLARE_TYPE(IntegralLBPExtraction)

  /*Node
   *
   * @name IntegralLBPExtraction
   * @category RobotFlow:Vision:FeaturesExtraction
   * @description Integral LBP (Local Binary Pattern) features extraction.
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
   * @parameter_name NUM_SAMPLES
   * @parameter_type int
   * @parameter_value 8
   * @parameter_description Number of samples in the circle in order to compute the LBP.
   *
   * @parameter_name PREDICATE
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Radius of the sample circle.
   *
   * @parameter_name DO_INTERPOLATION_FLAG
   * @parameter_type bool
   * @parameter_value false
   * @parameter_description Flag to use bilinear interpolation for sub-pixel samples.
   *
   * @parameter_name UNIFORM_PATTERNS_ONLY
   * @parameter_type bool
   * @parameter_value true
   * @parameter_description Use only rotation invariant uniform (riu2) patterns. (A false value will require an immense amount of memory!)
   *
   * @parameter_name PATTERN_INIT_ANGLE
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Starting angle for the pattern. Used only by the general LBP routine.
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

//
// General constants
//
static const double k_ILBP_pi = 3.14159265358979323846;
static const double k_ILBP_2pi = 2.0*k_ILBP_pi;

//
// Basic bilinear interpolation routine
//
#define interpolateAtPtr(u,i,c) ((m_BiLiMultipliers[(i<<2)] != 1.0) ? \
(*(u)*m_BiLiMultipliers[(i<<2)] + *(u+1)*m_BiLiMultipliers[(i<<2)+1] + *(u+c)*m_BiLiMultipliers[(i<<2)+2] + *(u+c+1)*m_BiLiMultipliers[(i<<2)+3] + 1e-10) : \
*(u))

//
// Interpolate than round result
//
#define intRound(u,i,c) int(interpolateAtPtr(u,i,c)+0.5)

//
// Default constructor for Object 
//
IntegralLBPExtraction::IntegralLBPExtraction()
: m_width(-1),
m_height(-1),
m_numChannels(-1),
m_numHoriIntRect(0),
m_numVertIntRect(0),
m_numSamples(0),
m_predicate(0),
m_doInterpolation(false),
m_useUniform(true),
m_startAngle(0),
m_maxValue(0),
m_featVect(NULL)
{

}

//
// Constructor with complete intialisation
//
IntegralLBPExtraction::IntegralLBPExtraction(int i_width, int i_height, 
	int i_numChannels, int i_numHoriIntRect, int i_numVertIntRect,
	int i_numSamples, int i_predicate, bool i_doInterpolation,
	bool i_useUniform, int i_startAngle, double i_maxValue, 
	bool i_useRectDiff)
: VisualFeaturesExtraction<double>(string("IntegralEdgesOriExtraction"), ParameterSet()),
m_width(i_width),
m_height(i_height),
m_numChannels(i_numChannels),
m_numHoriIntRect(i_numHoriIntRect),
m_numVertIntRect(i_numVertIntRect),
m_numSamples(i_numSamples),
m_predicate(i_predicate),
m_doInterpolation(i_doInterpolation),
m_useUniform(i_useUniform),
m_startAngle(i_startAngle),
m_maxValue(i_maxValue),
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
IntegralLBPExtraction::IntegralLBPExtraction(string nodeName, ParameterSet params)
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
	m_numSamples = dereference_cast<int>(parameters.get("NUM_SAMPLES"));
	m_predicate = dereference_cast<int>(parameters.get("PREDICATE"));
	m_doInterpolation = dereference_cast<bool>(parameters.get("DO_INTERPOLATION_FLAG"));
	m_useUniform = dereference_cast<bool>(parameters.get("UNIFORM_PATTERNS_ONLY"));
	m_startAngle = dereference_cast<int>(parameters.get("PATTERN_INIT_ANGLE"));
	m_useRectDiff = dereference_cast<bool>(parameters.get("USE_RECT_DIFF"));
	
	cout << "USE_RECT_DIFF=" << m_useRectDiff << endl;
	
	m_maxValue = 255.0;
	
	Initialize();
}
	
IntegralLBPExtraction::~IntegralLBPExtraction()
{
	delete [] m_samplePoints;
	delete [] m_pointsOffsets;
	delete [] m_BiLiMultipliers;
	delete [] m_tmpSamples;
	
	for (int l=0; l<m_numValidPattern; l++) {
		cvReleaseImage(&(m_sumImage[l]));
		cvReleaseImage(&(m_patternImage[l]));
	}
	
	cvReleaseImage(&m_grayImage);
	cvReleaseImage(&m_curImage);
	
	delete [] m_sumPixPtr;
	delete [] m_patternPixPtr;
	delete [] m_sumImage;
	delete [] m_patternImage;
	
	delete [] m_curMeanVal;
	delete [] m_tmpMeanFeatures;
}

// Modified BufferedNode request method to support cyclic node connection
void IntegralLBPExtraction::request(int output_id, const ParameterSet &req) 
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
		throw new GeneralException ("IntegralLBPExtraction::request : unknown output ID.",__FILE__,__LINE__);
	}

}

void IntegralLBPExtraction::calculate(int output_id, int count, Buffer &out)
{
	try {
		bool useNext = dereference_cast<bool>(getInput(m_useNextImgInID, count));
		
		if (useNext) {
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
		
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("IntegralLBPExtraction::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
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
				throw new GeneralException ("IntegralLBPExtraction::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Copy input image
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
			
			// Preprocess input image
			Preprocess(m_curImage);
			
			(*outputs[m_ppCompletedOutID].buffer)[count] = ObjectRef(Int::alloc(1));
		}
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in IntegralLBPExtraction::calculate:",__FILE__,__LINE__));
	}
}

void IntegralLBPExtraction::ExtractFeatures(VisualROI *i_roi)
{
	try {
		CvPoint ulcLimit, lrcLimit, curULC, curLRC;
		int roiWidth, roiHeight, deltaX, deltaY;
		double numPixelsInSubROI;
		int i,j,l;
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
					for (l=0; l<m_numValidPattern; l++) {
						// Compute mean color channel
						*p_tmpMeanFeatures = (double)(m_sumPixPtr[l][curLRC.y*m_imgSumWidth+curLRC.x] - 
							m_sumPixPtr[l][curLRC.y*m_imgSumWidth+curULC.x] - 
							m_sumPixPtr[l][curULC.y*m_imgSumWidth+curLRC.x] + 
							m_sumPixPtr[l][curULC.y*m_imgSumWidth+curULC.x])/numPixelsInSubROI;
							
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
				for (l=0; l<m_numValidPattern; l++) {
					m_curMeanVal[l] = m_tmpMeanFeatures[i*m_numValidPattern + l];
				}
				
				for (j=0; j<m_numIntRect; j++) {
					if (j != i) {
						for (l=0; l<m_numValidPattern; l++) {
							*featPtr++ = m_tmpMeanFeatures[j*m_numValidPattern + l] - m_curMeanVal[l] + m_maxValue;
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
					for (l=0; l<m_numValidPattern; l++) {
						// Compute mean color channel
						*featPtr++ = (double)(m_sumPixPtr[l][curLRC.y*m_imgSumWidth+curLRC.x] - 
							m_sumPixPtr[l][curLRC.y*m_imgSumWidth+curULC.x] - 
							m_sumPixPtr[l][curULC.y*m_imgSumWidth+curLRC.x] + 
							m_sumPixPtr[l][curULC.y*m_imgSumWidth+curULC.x])/numPixelsInSubROI;
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
		throw e->add(new GeneralException("Exception in IntegralLBPExtraction::ExtractFeatures:",__FILE__,__LINE__));
	}
}

void IntegralLBPExtraction::ExtractFeatures(IplImage *i_input, VisualROI *i_roi)
{
	try {
		Preprocess(i_input);
		
		ExtractFeatures(i_roi);
	}
	catch (BaseException *e) {
		throw e->add(new GeneralException("Exception in IntegralLBPExtraction::ExtractFeatures:",__FILE__,__LINE__));
	}
}

void IntegralLBPExtraction::Preprocess(IplImage *i_srcImg)
{
	int l;
	
	// Convert to graysacle
	if (m_numChannels == 3) {
		cvCvtColor(i_srcImg, m_grayImage, CV_BGR2GRAY);
	}
	else if (m_numChannels == 1) {
		// Copy input image
		memcpy(m_grayImage->imageData, i_srcImg->imageData, m_numBytesInFrame);
	}
	else {
		throw new GeneralException ("IntegralLBPExtraction::Preprocess : only images with 1 or 3 channels are supported.",__FILE__,__LINE__);
	}
	
	// Each local pattern is assigned a channel for integral image processing
	for (l=0; l<m_numValidPattern; l++) {
		// Set image values to zero
		cvZero(m_patternImage[l]);
		// Assign pointers to each orientation image
		m_patternPixPtr[l] = (unsigned char *)(m_patternImage[l]->imageData);
	}
	
	// Use appropriate function to compute the LBP
	(this->*m_extractionFct)((unsigned char*)(m_grayImage->imageData));
	
	// Preprocessing consists of computing the sum of pixels
	// in order to have the integral images.
	for (l=0; l<m_numValidPattern; l++) {
		cvIntegral(m_patternImage[l], m_sumImage[l], NULL, NULL);
		m_sumPixPtr[l] = (int *)(m_sumImage[l]->imageData);
	}
}

//
// Private mehtods
//

void IntegralLBPExtraction::Initialize()
{
	m_numPixels = m_width*m_height;
	m_numBytesInFrame = m_numPixels*m_numChannels;
	m_numIntRect = m_numHoriIntRect*m_numVertIntRect;
	if (m_useUniform) {
		m_numValidPattern = m_numSamples+1;
	}
	else {
		m_numValidPattern = 1 << m_numSamples;
	}
	
	int numIntRectFeatures;
	if (m_useRectDiff) {
		m_maxFeatValue = 2.0*m_maxValue;
		numIntRectFeatures = m_numIntRect + m_numIntRect*(m_numIntRect-1);
		m_tmpMeanFeatures = new double[m_numIntRect*m_numValidPattern];
		m_curMeanVal = new double[m_numValidPattern];
	}
	else {
		m_maxFeatValue = m_maxValue;
		numIntRectFeatures = m_numIntRect;
	}
	
	m_featVect = RCPtr<Vector<VisualFeatureDesc<double> *> >(Vector<VisualFeatureDesc<double> *>::alloc(1));
	(*m_featVect)[0] = new VisualIntegralDesc<double>(
		e_VISUALINTDESC_EuclideanDist, 
		m_numValidPattern, 
		numIntRectFeatures, 
		m_maxFeatValue, 
		m_useRectDiff,
		false);
	
	m_patternImage = new IplImage *[m_numValidPattern]; 
	m_sumImage = new IplImage *[m_numValidPattern];
	m_patternPixPtr = new unsigned char*[m_numValidPattern];
	m_sumPixPtr = new int *[m_numValidPattern];
	
	CvSize imgSize;
	imgSize.width = m_width;
	imgSize.height = m_height;
	m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
	m_grayImage = cvCreateImage(imgSize, IPL_DEPTH_8U, 1);
	
	// Sum images are of size (m_width+1)x(m_height+1)
	m_imgSumWidth = m_width+1;
	CvSize sumSize;
	sumSize.width = m_imgSumWidth;
	sumSize.height = m_height+1;
	
	for (int l=0; l<m_numValidPattern; l++) {
		m_patternImage[l] = cvCreateImage(imgSize, IPL_DEPTH_8U, 1);
		m_sumImage[l] = cvCreateImage(sumSize, IPL_DEPTH_32S, 1);
	}
	
	m_samplePoints = new CvPoint[m_numSamples];
	m_pointsOffsets = new CvPoint2D32f[m_numSamples];
	m_BiLiMultipliers = new double[4*m_numSamples];
	m_tmpSamples = new unsigned char *[m_numSamples];
	
	InitSamplePoints();
	
	// Assign appropriate private extraction routine
	if (m_numSamples == 8) {
		if (m_doInterpolation) {
			if (m_useUniform) {
				m_extractionFct = &IntegralLBPExtraction::ExtractLBP8RIU2WithInterpolation;
			}
			else {
				m_extractionFct = &IntegralLBPExtraction::ExtractLBP8WithInterpolation;
			}
		}
		else {
			if (m_useUniform) {
				m_extractionFct = &IntegralLBPExtraction::ExtractLBP8RIU2WithoutInterpolation;
			}
			else {
				m_extractionFct = &IntegralLBPExtraction::ExtractLBP8WithoutInterpolation;
			}
		}
	}
	else {
		if (m_doInterpolation) {
			if (m_useUniform) {
				m_extractionFct = &IntegralLBPExtraction::ExtractLBPGeneralRIU2WithInterpolation;
			}
			else {
				m_extractionFct = &IntegralLBPExtraction::ExtractLBPGeneralWithInterpolation;
			}
		}
		else {
			if (m_useUniform) {
				m_extractionFct = &IntegralLBPExtraction::ExtractLBPGeneralRIU2WithoutInterpolation;
			}
			else {
				m_extractionFct = &IntegralLBPExtraction::ExtractLBPGeneralWithoutInterpolation;
			}
		}
	}
}

//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
void IntegralLBPExtraction::InitSamplePoints()
{
	double step = k_ILBP_2pi / (double)m_numSamples;
		
	for (int i=0; i<m_numSamples; i++) {
		// Starting angle is used for more custom genreality
		double tmpX = m_predicate * cos(i * step + m_startAngle);
		double tmpY = m_predicate * sin(i * step + m_startAngle);
		
		m_samplePoints[i].x = (int)tmpX;
		m_samplePoints[i].y = (int)tmpY;
		m_pointsOffsets[i].x = tmpX - m_samplePoints[i].x;
		m_pointsOffsets[i].y = tmpY - m_samplePoints[i].y;
		
		//rounding error
		if (m_pointsOffsets[i].x < 1.0e-10 && m_pointsOffsets[i].x > -1.0e-10) {
			m_pointsOffsets[i].x = 0;
		}
		if (m_pointsOffsets[i].y < 1.0e-10 && m_pointsOffsets[i].y > -1.0e-10) {
			m_pointsOffsets[i].y = 0;
		}
		
		if (tmpX < 0 && m_pointsOffsets[i].x != 0) {
			m_samplePoints[i].x -= 1;
			m_pointsOffsets[i].x += 1;
		}
		if (tmpY < 0 && m_pointsOffsets[i].y != 0) {
			m_samplePoints[i].y -= 1;
			m_pointsOffsets[i].y += 1;
		}
		
		double dx = 1.0-m_pointsOffsets[i].x;
		double dy = 1.0-m_pointsOffsets[i].y;

		m_BiLiMultipliers[i*4+0] = dx*dy;
		m_BiLiMultipliers[i*4+1] = m_pointsOffsets[i].x*dy;
		m_BiLiMultipliers[i*4+2] = dx*m_pointsOffsets[i].y;
		m_BiLiMultipliers[i*4+3] = m_pointsOffsets[i].x*m_pointsOffsets[i].y;
	}
}

//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
// Specialized routine to extract a LBP using 8 samples
void IntegralLBPExtraction::ExtractLBP8WithInterpolation(unsigned char *i_pixPtr)
{
	int leap = m_width*m_predicate;
	const unsigned char
		*p0 = i_pixPtr,
		*p1 = p0 + m_predicate,
		*p2 = p1 + m_predicate,
		*p3 = p2 + leap,
		*p4 = p3 + leap,
		*p5 = p4 - m_predicate,
		*p6 = p5 - m_predicate,
		*p7 = p6 - leap,
		*center = p7 + m_predicate;
	int pred2 = 2*m_predicate;
	int limitY = m_height-pred2;
	int limitX = m_width-pred2;
	unsigned int value;
	int cntr;
	
	// Reset sub-pixel samples for interpolation
	p0 = center + m_samplePoints[5].x + m_samplePoints[5].y * m_width;
	p2 = center + m_samplePoints[7].x + m_samplePoints[7].y * m_width;
	p4 = center + m_samplePoints[1].x + m_samplePoints[1].y * m_width;
	p6 = center + m_samplePoints[3].x + m_samplePoints[3].y * m_width;
	
	for (int y=0; y<limitY; y++) {
		for (int x=0; x<limitX; x++) {
			value = 0;
			cntr = *center-1;
			
			// Unrolled loop for better efficiency
			// Exact pixel samples
			value |= ((unsigned int)(cntr - *p1++) & 0x80000000) >> 30;
			value |= ((unsigned int)(cntr - *p3++) & 0x80000000) >> 28;
			value |= ((unsigned int)(cntr - *p5++) & 0x80000000) >> 26;
			value |= ((unsigned int)(cntr - *p7++) & 0x80000000) >> 24;
			
			// Interpolate for sub-pixel samples
			value |= ((unsigned int)(cntr - intRound(p0,5,m_width)) & 0x80000000) >> 31;
			p0++;
			value |= ((unsigned int)(cntr - intRound(p2,7,m_width)) & 0x80000000) >> 29;
			p2++;
			value |= ((unsigned int)(cntr - intRound(p4,1,m_width)) & 0x80000000) >> 27;
			p4++;
			value |= ((unsigned int)(cntr - intRound(p6,3,m_width)) & 0x80000000) >> 25;
			p6++;
			center++;
			
			if (value < m_numValidPattern) {
				m_patternPixPtr[value][y*m_width+x] = 255;
			}
		}
		
		p0 += pred2;
		p1 += pred2;
		p2 += pred2;
		p3 += pred2;
		p4 += pred2;
		p5 += pred2;
		p6 += pred2;
		p7 += pred2;
		center += pred2;
	}
}
	
//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
void IntegralLBPExtraction::ExtractLBP8WithoutInterpolation(unsigned char *i_pixPtr)
{
	int leap = m_width*m_predicate;
	const unsigned char
		*p0 = i_pixPtr,
		*p1 = p0 + m_predicate,
		*p2 = p1 + m_predicate,
		*p3 = p2 + leap,
		*p4 = p3 + leap,
		*p5 = p4 - m_predicate,
		*p6 = p5 - m_predicate,
		*p7 = p6 - leap,
		*center = p7 + m_predicate;
	int pred2 = 2*m_predicate;
	int limitY = m_height-pred2;
	int limitX = m_width-pred2;
	unsigned int value;
	int cntr;
	
	for (int y=0; y<limitY; y++) {
		for (int x=0; x<limitX; x++) {
			value = 0;
			cntr = *center-1;
			
			// Unrolled loop for better efficiency
			value |= ((unsigned int)(cntr - *p0++) & 0x80000000) >> 31;
			value |= ((unsigned int)(cntr - *p1++) & 0x80000000) >> 30;
			value |= ((unsigned int)(cntr - *p2++) & 0x80000000) >> 29;
			value |= ((unsigned int)(cntr - *p3++) & 0x80000000) >> 28;
			value |= ((unsigned int)(cntr - *p4++) & 0x80000000) >> 27;
			value |= ((unsigned int)(cntr - *p5++) & 0x80000000) >> 26;
			value |= ((unsigned int)(cntr - *p6++) & 0x80000000) >> 25;
			value |= ((unsigned int)(cntr - *p7++) & 0x80000000) >> 24;
			center++;

			if (value < m_numValidPattern) {
				m_patternPixPtr[value][y*m_width+x] = 255;
			}
		}
		
		p0 += pred2;
		p1 += pred2;
		p2 += pred2;
		p3 += pred2;
		p4 += pred2;
		p5 += pred2;
		p6 += pred2;
		p7 += pred2;
		center += pred2;
	}
}

//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
void IntegralLBPExtraction::ExtractLBP8RIU2WithInterpolation(unsigned char *i_pixPtr)
{
	int leap = m_width*m_predicate;
	const unsigned char
		*p0 = i_pixPtr,
		*p1 = p0 + m_predicate,
		*p2 = p1 + m_predicate,
		*p3 = p2 + leap,
		*p4 = p3 + leap,
		*p5 = p4 - m_predicate,
		*p6 = p5 - m_predicate,
		*p7 = p6 - leap,
		*center = p7 + m_predicate;
	int pred2 = 2*m_predicate;
	int limitY = m_height-pred2;
	int limitX = m_width-pred2;
	unsigned int value;
	int cntr;
	
	// Reset sub-pixel samples for interpolation
	p0 = center + m_samplePoints[5].x + m_samplePoints[5].y * m_width;
	p2 = center + m_samplePoints[7].x + m_samplePoints[7].y * m_width;
	p4 = center + m_samplePoints[1].x + m_samplePoints[1].y * m_width;
	p6 = center + m_samplePoints[3].x + m_samplePoints[3].y * m_width;
	
	for (int y=0; y<limitY; y++) {
		for (int x=0; x<limitX; x++) {
			value = 0;
			cntr = *center-1;
			
			// Unrolled loop for better efficiency
			// Exact pixel samples
			value |= ((unsigned int)(cntr - *p1++) & 0x80000000) >> 30;
			value |= ((unsigned int)(cntr - *p3++) & 0x80000000) >> 28;
			value |= ((unsigned int)(cntr - *p5++) & 0x80000000) >> 26;
			value |= ((unsigned int)(cntr - *p7++) & 0x80000000) >> 24;
			
			// Interpolate for sub-pixel samples
			value |= ((unsigned int)(cntr - intRound(p0,5,m_width)) & 0x80000000) >> 31;
			p0++;
			value |= ((unsigned int)(cntr - intRound(p2,7,m_width)) & 0x80000000) >> 29;
			p2++;
			value |= ((unsigned int)(cntr - intRound(p4,1,m_width)) & 0x80000000) >> 27;
			p4++;
			value |= ((unsigned int)(cntr - intRound(p6,3,m_width)) & 0x80000000) >> 25;
			p6++;
			center++;
			
			if (ComputeBitTransitions(value) <= 2) {
				m_patternPixPtr[CountOneBits(value)][y*m_width+x] = 255;
			}
		}
		
		p0 += pred2;
		p1 += pred2;
		p2 += pred2;
		p3 += pred2;
		p4 += pred2;
		p5 += pred2;
		p6 += pred2;
		p7 += pred2;
		center += pred2;
	}
}

//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
void IntegralLBPExtraction::ExtractLBP8RIU2WithoutInterpolation(unsigned char *i_pixPtr)
{
	int leap = m_width*m_predicate;
	const unsigned char
		*p0 = i_pixPtr,
		*p1 = p0 + m_predicate,
		*p2 = p1 + m_predicate,
		*p3 = p2 + leap,
		*p4 = p3 + leap,
		*p5 = p4 - m_predicate,
		*p6 = p5 - m_predicate,
		*p7 = p6 - leap,
		*center = p7 + m_predicate;
	int pred2 = 2*m_predicate;
	int limitY = m_height-pred2;
	int limitX = m_width-pred2;
	unsigned int value;
	int cntr;
	
	for (int y=0; y<limitY; y++) {
		for (int x=0; x<limitX; x++) {
			value = 0;
			cntr = *center-1;
			
			// Unrolled loop for better efficiency
			value |= ((unsigned int)(cntr - *p0++) & 0x80000000) >> 31;
			value |= ((unsigned int)(cntr - *p1++) & 0x80000000) >> 30;
			value |= ((unsigned int)(cntr - *p2++) & 0x80000000) >> 29;
			value |= ((unsigned int)(cntr - *p3++) & 0x80000000) >> 28;
			value |= ((unsigned int)(cntr - *p4++) & 0x80000000) >> 27;
			value |= ((unsigned int)(cntr - *p5++) & 0x80000000) >> 26;
			value |= ((unsigned int)(cntr - *p6++) & 0x80000000) >> 25;
			value |= ((unsigned int)(cntr - *p7++) & 0x80000000) >> 24;
			center++;

			if (ComputeBitTransitions(value) <= 2) {
				m_patternPixPtr[CountOneBits(value)][y*m_width+x] = 255;
			}
		}
		
		p0 += pred2;
		p1 += pred2;
		p2 += pred2;
		p3 += pred2;
		p4 += pred2;
		p5 += pred2;
		p6 += pred2;
		p7 += pred2;
		center += pred2;
	}
}

//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
// General routine to extract a LBP using a number of samples
//  different from the supported specialized routines
void IntegralLBPExtraction::ExtractLBPGeneralWithInterpolation(unsigned char *i_pixPtr)
{
	int i,x,y;
	unsigned int value, base;
	int pred2 = 2*m_predicate;
	int limitY = m_height-pred2;
	int limitX = m_width-pred2;
	
	unsigned char* center = i_pixPtr + m_predicate * (1 + m_width);
	
	for (i=0; i<m_numSamples; i++) {
		m_tmpSamples[i] = center + m_samplePoints[i].x  + m_samplePoints[i].y * m_width;
	}

	for (y=0; y<limitY; y++) {
		for (x=0; x<limitX; x++) {
			value = 0;
			base = 1;
			for (i=0;i<m_numSamples;i++) {
				if (interpolateAtPtr(m_tmpSamples[i],i,m_width) >= *center) {
					value |= base;
				}
					
				base <<= 1;
				m_tmpSamples[i]++;
			}
				
			center++;

			if (value < m_numValidPattern) {
				m_patternPixPtr[value][y*m_width+x] = 255;
			}
		}
				
		for (i=0;i<m_numSamples;i++) {
			m_tmpSamples[i] += pred2;
		}
		
		center += pred2;
	}
}

//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
void IntegralLBPExtraction::ExtractLBPGeneralWithoutInterpolation(unsigned char *i_pixPtr)
{
	int i,x,y;
	unsigned int value, base;
	int pred2 = 2*m_predicate;
	int limitY = m_height-pred2;
	int limitX = m_width-pred2;
	
	unsigned char* center = i_pixPtr + m_predicate * (1 + m_width);
	
	for (i=0; i<m_numSamples; i++) {
		int xc = (int)round((double)m_samplePoints[i].x + m_pointsOffsets[i].x);
		int yc = (int)round((double)m_samplePoints[i].y + m_pointsOffsets[i].y);
		m_tmpSamples[i] = center + xc + yc * m_width;
	}

	for (y=0; y<limitY; y++) {
		for (x=0; x<limitX; x++) {
			value = 0;
			base = 1;
			for (i=0;i<m_numSamples;i++) {
				if (*m_tmpSamples[i] >= *center) {
					value |= base;
				}
					
				base <<= 1;
				m_tmpSamples[i]++;
			}
				
			center++;

			if (value < m_numValidPattern) {
				m_patternPixPtr[value][y*m_width+x] = 255;
			}
		}
				
		for (i=0;i<m_numSamples;i++) {
			m_tmpSamples[i] += pred2;
		}
		
		center += pred2;
	}
}

//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
void IntegralLBPExtraction::ExtractLBPGeneralRIU2WithInterpolation(unsigned char *i_pixPtr)
{
	int i,x,y;
	unsigned int value, base;
	int pred2 = 2*m_predicate;
	int limitY = m_height-pred2;
	int limitX = m_width-pred2;
	
	unsigned char* center = i_pixPtr + m_predicate * (1 + m_width);
	
	for (i=0; i<m_numSamples; i++) {
		m_tmpSamples[i] = center + m_samplePoints[i].x  + m_samplePoints[i].y * m_width;
	}

	for (y=0; y<limitY; y++) {
		for (x=0; x<limitX; x++) {
			value = 0;
			base = 1;
			for (i=0;i<m_numSamples;i++) {
				if (interpolateAtPtr(m_tmpSamples[i],i,m_width) >= *center) {
					value |= base;
				}
					
				base <<= 1;
				m_tmpSamples[i]++;
			}
				
			center++;

			if (ComputeBitTransitions(value) <= 2) {
				m_patternPixPtr[CountOneBits(value)][y*m_width+x] = 255;
			}
		}
				
		for (i=0;i<m_numSamples;i++) {
			m_tmpSamples[i] += pred2;
		}
		
		center += pred2;
	}
}

//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
void IntegralLBPExtraction::ExtractLBPGeneralRIU2WithoutInterpolation(unsigned char *i_pixPtr)
{
	int i,x,y;
	unsigned int value, base;
	int pred2 = 2*m_predicate;
	int limitY = m_height-pred2;
	int limitX = m_width-pred2;
	
	unsigned char* center = i_pixPtr + m_predicate * (1 + m_width);

	for (unsigned int i=0; i<m_numSamples; i++) {
		m_tmpSamples[i] = center + m_samplePoints[i].x + m_samplePoints[i].y * m_width;
	}
	
	for (i=0; i<m_numSamples; i++) {
		int xc = (int)round((double)m_samplePoints[i].x + m_pointsOffsets[i].x);
		int yc = (int)round((double)m_samplePoints[i].y + m_pointsOffsets[i].y);
		m_tmpSamples[i] = center + xc + yc * m_width;
	}

	for (y=0; y<limitY; y++) {
		for (x=0; x<limitX; x++) {
			value = 0;
			base = 1;
			for (i=0;i<m_numSamples;i++) {
				if (*m_tmpSamples[i] >= *center) {
					value |= base;
				}
					
				base <<= 1;
				m_tmpSamples[i]++;
			}
				
			center++;

			if (ComputeBitTransitions(value) <= 2) {
				m_patternPixPtr[CountOneBits(value)][y*m_width+x] = 255;
			}
		}
				
		for (i=0;i<m_numSamples;i++) {
			m_tmpSamples[i] += pred2;
		}
		
		center += pred2;
	}
}
	
//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
int IntegralLBPExtraction::ComputeBitTransitions(unsigned int i_val)
{
	int base = 1;
	unsigned int current = i_val & base;
	unsigned int current2; 
	int changes = 0;
	
	for (int i=1;i<m_numSamples;i++) {
		base <<= 1;
		current2 = (i_val & base) >> i;
		if (current ^ current2) changes++;
		current = current2;
	}
	
	return changes;
}
	
//
// Code adapted from
// Cpplibs C++ libraries and PRAPI
// Copyright (C) 2001 Topi Mäenpää and Jaakko Viertola
//
int IntegralLBPExtraction::CountOneBits(unsigned int i_val)
{
	int count = 0;
	unsigned int base = 1;
	
	for (int i=0;i<m_numSamples;i++) {
		if (i_val & base) count++;
		base <<= 1;
	}
	
	return count;
}

}//namespace RobotFlow
