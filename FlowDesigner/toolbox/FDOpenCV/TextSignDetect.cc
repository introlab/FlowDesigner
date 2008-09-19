#ifndef _TEXTSIGNDETECT_CC_
#define _TEXTSIGNDETECT_CC_

#include "BufferedNode.h"
#include "cv.h"
#include "Image.h"
#include "VisualROI.h"
#include "IntegralLBPExtraction.h"
#include "Vector.h"
#include <stdlib.h>
#include <sys/timeb.h>

using namespace std;
using namespace FD;

namespace RobotFlow {

  class TextSignDetect;

  DECLARE_NODE(TextSignDetect)

  /*Node
   *
   * @name TextSignDetect
   * @category RobotFlow:Vision:OpenCV
   * @description Determine the contours in the input image.
   *
   * @input_name IN_IMAGE
   * @input_type Image
   * @input_description Current color frame to process.
   *
   * @input_name IN_TRACKED_ROI
   * @input_type VisualROI
   * @input_description Region of interest currently tracked.
   *
   * @input_name ACTIVATION
   * @input_type bool
   * @input_description processing activation flag
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
   * @parameter_description Video frame number of channels.
   *
   * @parameter_name NUM_ORIENTATIONS
   * @parameter_type int
   * @parameter_value 6
   * @parameter_description Number of bins for the local edge histogram.
   *
   * @parameter_name EDGES_STRENGTH_THRESHOLD
   * @parameter_type float
   * @parameter_value 50.0
   * @parameter_description Threshold to remove noisy/weak edges.
   *
   * @parameter_name TEXT_ROI_MIN_WIDTH
   * @parameter_type int
   * @parameter_value 10
   * @parameter_description Minimal width of a potential region of interest to consider it as text.
   *
   * @parameter_name TEXT_ROI_MIN_HEIGHT
   * @parameter_type int
   * @parameter_value 4
   * @parameter_description Minimal height of a potential region of interest to consider it as text.
   *
   * @parameter_name TEXT_ROI_MAX_WIDTH
   * @parameter_type int
   * @parameter_value 320
   * @parameter_description Maximal width of a potential region of interest to consider it as text.
   *
   * @parameter_name TEXT_ROI_MAX_HEIGHT
   * @parameter_type int
   * @parameter_value 80
   * @parameter_description Maximal height of a potential region of interest to consider it as text.
   *
   * @parameter_name MAX_STD_ORIENTATION_BINS
   * @parameter_type float
   * @parameter_value 0.75
   * @parameter_description Maximal standard deviation of the local edge orientation histogram bins heightEdges orientation histogram of a potential region of interest to consider it as text.
   *
   * @parameter_name MAX_NUM_TEXT_ROI
   * @parameter_type int
   * @parameter_value 100
   * @parameter_description Maximum number of text region of interest to use.
   *
   * @parameter_name MAX_STD
   * @parameter_type float
   * @parameter_value 0
   * @parameter_description Low threshold used for edge searching.
   *
   * @parameter_name MIN_DIST
   * @parameter_type float
   * @parameter_value 60
   * @parameter_description High threshold used for edge searching.
   *
   * @parameter_name HORIZONTAL_SYMMETRY_THRESHOLD
   * @parameter_type float
   * @parameter_value 80.0
   * @parameter_description Horizontal edge orientation symmetry treshold. Text region should have lower values than threshold.
   *
   * @parameter_name VERTICAL_SYMMETRY_THRESHOLD
   * @parameter_type float
   * @parameter_value 80.0
   * @parameter_description Vertical edge orientation symmetry treshold. Text region should have lower values than threshold.
   *
   * @output_name TEXT_ROI_IMG
   * @output_type Image
   * @output_description Image with potential text region of interest drawn.
   *
   * @output_name OUT_ROI
   * @output_type VisualROI
   * @output_description Region of interest corresponding to largest region of text.
   *
   * @output_name EDGES_ORI_SUM_IMG
   * @output_type Vector<double *>
   * @output_description Reference to edges orientation integral images. 
   *
   END*/

    static const double k_CVFC_pi = 3.14159265358979323846;
  static const double k_CVFC_2piInv = 1.0/(2.0*k_CVFC_pi);
   
  class TextSignDetect : public BufferedNode {

  public:

    TextSignDetect(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
    {
      m_imageInID = addInput("IN_IMAGE");
	  m_roiInID = addInput("IN_TRACKED_ROI");
	  m_activationInID = addInput("ACTIVATION");
	  
      m_imageOutID = addOutput("TEXT_ROI_IMG");
      m_roiOutID = addOutput("OUT_ROI");
	  m_edgesOriOutID = addOutput("EDGES_ORI_SUM_IMG");

      m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
      m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
      m_numChannels = dereference_cast<int>(parameters.get("NUM_CHANNELS"));
      m_numOriBins = dereference_cast<int>(parameters.get("NUM_ORIENTATIONS"));
      m_edgesStrTresh = dereference_cast<float>(parameters.get("EDGES_STRENGTH_THRESHOLD"));
      m_textROIMinWidth = dereference_cast<int>(parameters.get("TEXT_ROI_MIN_WIDTH"));
      m_textROIMinHeight = dereference_cast<int>(parameters.get("TEXT_ROI_MIN_HEIGHT"));
      m_textROIMaxWidth = dereference_cast<int>(parameters.get("TEXT_ROI_MAX_WIDTH"));
      m_textROIMaxHeight = dereference_cast<int>(parameters.get("TEXT_ROI_MAX_HEIGHT"));
      m_edgesOriMaxBinsStd = dereference_cast<float>(parameters.get("MAX_STD_ORIENTATION_BINS"));
      m_maxNumTextROI = dereference_cast<int>(parameters.get("MAX_NUM_TEXT_ROI"));
      m_minDist = dereference_cast<float>(parameters.get("MIN_DIST"));
      m_maxStd =  dereference_cast<float>(parameters.get("MAX_STD"));
      m_horiSymThresh = dereference_cast<float>(parameters.get("HORIZONTAL_SYMMETRY_THRESHOLD"));
      m_vertSymThresh =  dereference_cast<float>(parameters.get("VERTICAL_SYMMETRY_THRESHOLD"));
		
      m_numPixels = m_width*m_height;
      m_numBytesPerFrame = m_numPixels*m_numChannels;
		
      CvSize imgSize;
      imgSize.width = m_width;
      imgSize.height = m_height;
      m_src = cvCreateImage( imgSize, IPL_DEPTH_8U, m_numChannels );
      m_gray = cvCreateImage( imgSize, IPL_DEPTH_8U, 1 );
      m_mask = cvCreateImage( imgSize, IPL_DEPTH_8U, 1 );
      m_edges = cvCreateImage( imgSize, IPL_DEPTH_8U, 1 );
      m_binarized = cvCreateImage( imgSize, IPL_DEPTH_8U, 1 );
	  m_tmpBinarized = cvCreateImage( imgSize, IPL_DEPTH_8U, 1 );
      m_oriXImage = cvCreateImage( imgSize, IPL_DEPTH_16S, 1 );
      m_oriYImage = cvCreateImage( imgSize, IPL_DEPTH_16S, 1 );
		
      CvSize sumImgSize;
      sumImgSize.width = m_width+1;
      sumImgSize.height = m_height+1;
		
      m_edgesOri = new IplImage*[m_numOriBins];
      m_edgesOriSum = new IplImage*[m_numOriBins];
      m_edgesOriPix = new float*[m_numOriBins];
      m_edgesOriSumPix = new double*[m_numOriBins];
		
      m_tmpOriVal = new double[m_numOriBins];
	  
	  m_edgesOriSumRef = RCPtr<Vector<double *> >(Vector<double *>::alloc(m_numOriBins));
	  
      for (int b=0; b<m_numOriBins; b++) {
	m_edgesOri[b] = cvCreateImage( imgSize, IPL_DEPTH_32F, 1 );
	m_edgesOriSum[b] = cvCreateImage( sumImgSize, IPL_DEPTH_64F, 1 );
	(*m_edgesOriSumRef)[b] = (double *)(m_edgesOriSum[b]->imageData);
      }
		
      m_textROI = new CvRect[m_maxNumTextROI];
      m_finalTextROI = new CvRect[m_maxNumTextROI];
      m_labels = new int[m_maxNumTextROI];
		
      m_contourStorage = cvCreateMemStorage(0);
	  
      m_outROI = RCPtr<VisualROI>(new VisualROI(e_VISUALROI_rectangular, 1, 1, 1, 1, 0));
	  
	  m_intLBPExtract = new IntegralLBPExtraction(m_width, m_height, 
		m_numChannels, 1, 1, 8, 1, false, true, 0, 255.0, false);
    }
	
    ~TextSignDetect()
    {
		delete m_intLBPExtract;
      delete [] m_labels;
      delete [] m_finalTextROI;
      delete [] m_textROI;
		
      for (int b=0; b<m_numOriBins; b++) {
	cvReleaseImage(&m_edgesOriSum[b]);
	cvReleaseImage(&m_edgesOri[b]);
      }
      delete [] m_tmpOriVal;
      delete [] m_edgesOriSumPix;
      delete [] m_edgesOriPix;
      delete [] m_edgesOriSum;
      delete [] m_edgesOri;
		
      cvReleaseImage(&m_oriYImage);
      cvReleaseImage(&m_oriXImage);
      cvReleaseImage(&m_mask);
      cvReleaseImage(&m_gray);
      cvReleaseImage(&m_src);
      cvReleaseImage(&m_edges);
      cvReleaseImage(&m_binarized);
	  cvReleaseImage(&m_tmpBinarized);
    }


    void calculate(int output_id, int count, Buffer &out)
    {
      try {

	RCPtr<Bool> activation = getInput(m_activationInID,count);

	if (!*activation) {
	  out[count] = nilObject;
	  return;
	}
 

	//struct timeb t1, t2;
	int b, l, p, r;
			
	// Get input image
	RCPtr<Image> imageRef = getInput(m_imageInID, count);
			
	// Verify input image sanity
	if (imageRef->get_width() != m_width ||
	    imageRef->get_height() != m_height ||
	    imageRef->get_pixelsize() != m_numChannels) {
	  throw new GeneralException ("TextSignDetect::calculate : image size mismatch",__FILE__,__LINE__);
	}
	
	cvResetImageROI(m_src);
	cvResetImageROI(m_mask);
	cvResetImageROI(m_gray);
	cvResetImageROI(m_binarized);
	cvResetImageROI(m_edges);
	
	// Start timer
	//ftime(&t1);
			
	if (m_numChannels == 1) {
	  // Copy directly input image
	  memcpy(m_gray->imageData, imageRef->get_data(), m_numBytesPerFrame);
	}
	else if (m_numChannels == 3) {
	  // Copy input image
	  memcpy(m_src->imageData, imageRef->get_data(), m_numBytesPerFrame);
				
	  // Convert to grayscale
	  cvCvtColor(m_src, m_gray, CV_BGR2GRAY);
	}
	else {
	  throw new GeneralException ("TextSignDetect::calculate : only images with 1 or 3 channel(s) are supported.",__FILE__,__LINE__);
	}
			
	// Apply gaussian smoothing to reduce noise effect
	cvSmooth(m_gray,m_gray,CV_GAUSSIAN,3,3);
			
	// Apply a Sobel filter to get x derivatives
	cvSobel(m_gray, m_oriXImage, 1, 0, 3);

	// Apply a Sobel filter to get y derivatives
	cvSobel(m_gray, m_oriYImage, 0, 1, 3);
			
	// Pointers to derivatives values and edges image
	signed short *p_oriXPixels = (signed short *)(m_oriXImage->imageData);
	signed short *p_oriYPixels = (signed short *)(m_oriYImage->imageData);
	unsigned char *p_edgesPix = (unsigned char *)(m_edges->imageData);
			
	// Each orientation is assigned a channel for integral image processing
	for (b=0; b<m_numOriBins; b++) {
	  // Assign pointers to each orientation image
	  m_edgesOriPix[b] = (float *)(m_edgesOri[b]->imageData);
	}
			
	//
	// Process each pixel location to build edges orientation channels
	//
	for (p=0; p<m_numPixels; p++) {
	  // Compute edge strength
	  float strength = sqrt((float)((*p_oriXPixels)*(*p_oriXPixels) + (*p_oriYPixels)*(*p_oriYPixels)));
	  // Compute edge orientation
	  float angle = atan2((float)(*p_oriYPixels), (float)(*p_oriXPixels)) + k_CVFC_pi;
				
	  // Threshold weak/noisy edges
	  if (strength < m_edgesStrTresh) {
	    strength = 0.f;
	    *p_edgesPix++ = 0;
	  }
	  else {
	    // Build binary image for connected components processing
	    *p_edgesPix++ = 255;
	  }
				
	  // Determine the orientation image channel
	  int angleChannel = cvRound((angle*k_CVFC_2piInv)*(float)(m_numOriBins-1));
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
				
	  // Increase pointer for next pixel location
	  p_oriXPixels++;
	  p_oriYPixels++;
	}
			
	//
	// Compute integral images on all orientation channels
	//
	for (b=0; b<m_numOriBins; b++) {
	  cvIntegral(m_edgesOri[b], m_edgesOriSum[b], NULL, NULL);
	}
			
	// Assign pointers on each orientation integral image
	for (b=0; b<m_numOriBins; b++) {
	  m_edgesOriSumPix[b] = (double *)(m_edgesOriSum[b]->imageData);
	}
			
	// Contour (connected compnents) sequence
	CvSeq *contour;
			
	//
	// Find connected components contours using a simple chain code approximation
	//	  
	cvFindContours(m_edges, m_contourStorage, &contour, sizeof(CvContour), CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE);
			
	// Initialize the current number of potential text region of interest
	int curNumTextROI = 0;
	// Width of integral image is always one pixel greater than the original image
	int sumWidth = m_width+1;
			
	//
	// Process each contour to determine if it is a potential text region of interest
	//
	for( ; contour != 0; contour = contour->h_next ) {
	  // Get rectangular bounding box of current contour
	  CvRect bbox = cvContourBoundingRect(contour, 1);
				
	  // Use only region with valid width and height
	  if (bbox.width > m_textROIMinWidth && bbox.width < m_textROIMaxWidth &&
	      bbox.height > m_textROIMinHeight && bbox.height < m_textROIMaxHeight) {
	    // Verify local edge orientation histogram in current region
	    CvPoint pt1, pt2;
	    pt1.x = bbox.x;
	    pt1.y = bbox.y;
	    pt2.x = pt1.x + bbox.width;
	    pt2.y = pt1.y + bbox.height;
					
	    // First compute the total edge orientation values
	    double oriValSum = 0.0;
	    for (b=0; b<m_numOriBins; b++) {
	      m_tmpOriVal[b] =  m_edgesOriSumPix[b][pt2.y*sumWidth+pt2.x] - m_edgesOriSumPix[b][pt2.y*sumWidth+pt1.x] - 
		m_edgesOriSumPix[b][pt1.y*sumWidth+pt2.x] + m_edgesOriSumPix[b][pt1.y*sumWidth+pt1.x];
						
	      oriValSum += m_tmpOriVal[b];
	    }
					
	    // Only consider region with some edges
	    if (oriValSum > 0.0) {
	      double meanBinHeight = 0.0;
	      double stdBinHeight = 0.0;
						
	      // Compute local edge orientation histogram mean bin height
	      for (b=0; b<m_numOriBins; b++) {
		meanBinHeight += m_tmpOriVal[b]/oriValSum;
	      }
	      meanBinHeight /= m_numOriBins;
						
	      // Compute local edge orientation histogram bin height standard deviation
	      for (b=0; b<m_numOriBins; b++) {
		double diff = ((m_tmpOriVal[b])/(oriValSum)-meanBinHeight);
		stdBinHeight += diff*diff;
	      }
	      stdBinHeight = sqrt(stdBinHeight);
						
	      // Draw current region (for debug)
	      //cvRectangle(m_src, pt1, pt2, CV_RGB(255,255,255), 1);
							
	      int deltaX = bbox.width / m_numOriBins;
						
	      // Verify that current region has a valid local edge
	      // orientation histogram bin height standard deviation.
	      // Note: for text region, the histogram has an almost
	      // flat orientation distribution.
	      if (stdBinHeight < m_edgesOriMaxBinsStd) {
		// Valid text region
		m_textROI[curNumTextROI++] = bbox;
	      }
	    }
	  }
	}
			
	// Clear contour information
	cvClearMemStorage(m_contourStorage);
			
	double threshold;
	int numFinalTextROI = 0;
	for (r=0; r<curNumTextROI; r++) {
				
	  if (processRect(m_textROI[r], m_src, NULL, m_maxStd, m_minDist, &threshold )) {
	    // Valid text region
	    //cvRectangle(m_src, ulc, lrc, CV_RGB(255,0,0), 1);
	    m_finalTextROI[numFinalTextROI++] = m_textROI[r];
	  }
	}
			
	//
	// Find and group neighboring and similar region of text 
	//
	// Init labels
	for (r=0; r<numFinalTextROI; r++) {
	  m_labels[r] = 0;
	}
			
	int curLabel = 1;
	for (r=0; r<numFinalTextROI; r++) {
	  CvRect curROI = m_finalTextROI[r];
	  int widthThres = cvRound((float)(curROI.width)*0.1f);
	  int heightThres = cvRound((float)(curROI.height)*0.1f);
	  int posXThres = cvRound((float)(curROI.width)*1.5f);
	  int posYThres = cvRound((float)(curROI.height)*1.5f);
	  for (int r2=0; r2<numFinalTextROI; r2++) {
	    if (r2 == r) {
	      continue;
	    }
					
	    if (abs(m_finalTextROI[r2].width-curROI.width) < widthThres ||
		abs(m_finalTextROI[r2].height-curROI.height) < heightThres) {
	      int distX = abs(m_finalTextROI[r2].x-curROI.x);
	      int distY = abs(m_finalTextROI[r2].y-curROI.y);
	      if (distX < posXThres &&
		  distY < posYThres) {
		// Both region match
		if (!m_labels[r] && !m_labels[r2]) {
		  m_labels[r] = m_labels[r2] = curLabel++;
		}
		else if (m_labels[r] && !m_labels[r2]) {
		  m_labels[r2] = m_labels[r];
		}
		else if (!m_labels[r] && m_labels[r2]) {
		  m_labels[r] = m_labels[r2];
		}
		else {
		  // Mark all r2 labels as r
		  int equiLabel = m_labels[r2];
		  for (int r3=0; r3<numFinalTextROI; r3++) {
		    if (m_labels[r3] == equiLabel) {
		      m_labels[r3] = m_labels[r];
		    }
		  }
		}
	      }
	    }
	  }
				
	  if (!m_labels[r]) {
	    m_labels[r] = curLabel++;
	  }
	}
	
	// Process all labels
	for (l=0; l<curLabel; l++) {
	  CvPoint ulc;
	  CvPoint lrc;
	  bool roiInit = false;
	  int rightX, lowerY;
	  int labelOfInterest = m_labels[l];
				
	  for (r=0; r<numFinalTextROI; r++) {
	    if (m_labels[r] == labelOfInterest) {
	      if (roiInit) {
		if (m_finalTextROI[r].x < ulc.x) {
		  ulc.x = m_finalTextROI[r].x;
		}
							
		if (m_finalTextROI[r].y < ulc.y) {
		  ulc.y = m_finalTextROI[r].y;
		}
							
		rightX = m_finalTextROI[r].x+m_finalTextROI[r].width;
		if (rightX > lrc.x) {
		  lrc.x = rightX;
		}
							
		lowerY = m_finalTextROI[r].y+m_finalTextROI[r].height;
		if (lowerY > lrc.y) {
		  lrc.y = lowerY;
		}
	      }
	      else {
		ulc.x = m_finalTextROI[r].x;
		ulc.y = m_finalTextROI[r].y;
		lrc.x = ulc.x + m_finalTextROI[r].width;
		lrc.y = ulc.y + m_finalTextROI[r].height;
		roiInit = true;
	      }
	    }
	  }
				
	  //cvRectangle(m_src, ulc, lrc, CV_RGB(255,0,0), 1);
	  m_textROI[l].x = ulc.x;
	  m_textROI[l].y = ulc.y;
	  m_textROI[l].width = lrc.x - ulc.x;
	  m_textROI[l].height = lrc.y - ulc.y;
	}
	
	// See if we are purely detecting or 
	// if we are validating a tracked ROI
	bool pureDetect = true;
	ObjectRef roiRef = getInput(m_roiInID, count);
	RCPtr<VisualROI> trackedROI;
	int trackedXCen;
	int trackedYCen;
	
	if (!roiRef->isNil()) {
		pureDetect = false;
		trackedROI = roiRef;
		trackedXCen = trackedROI->GetXCen();
		trackedYCen = trackedROI->GetYCen();
	}
	
	//
	// Verify texture of region
	//
	int curNumLabels = curLabel-1;
	int maxRegionSize = 0;
	int curRegionIdx = -1;
	m_intLBPExtract->Preprocess(m_src);
	for (l=0; l<curNumLabels; l++) {
		CvPoint ulc1, lrc1;
		int hsx = m_textROI[l].width/2;
		int hsy = m_textROI[l].height/2;
		m_outROI->SetXCen(m_textROI[l].x + hsx);
		m_outROI->SetYCen(m_textROI[l].y + hsy);
		m_outROI->Reset(hsx, hsy, 0);
		
		m_intLBPExtract->ExtractFeatures(m_outROI.get());
		
		double *lbpFeats = m_intLBPExtract->GetDescriptor()->GetFeatures();
		
		double minLBP = 10000.0;
		double secMinLBP = 10000.0;
		double meanLBP = 0.0;
		for (int i=0; i<9; i++) {
			meanLBP += lbpFeats[i];
			
			if (minLBP > lbpFeats[i]) {
				secMinLBP = minLBP;
				minLBP = lbpFeats[i];
			}
			else if (secMinLBP > lbpFeats[i]) {
				secMinLBP = lbpFeats[i];
			}
		}
		
		meanLBP /= 9.0;
		
		double varLBP = 0.0;
		for (int i=0; i<9; i++) {
			double diff = lbpFeats[i] - meanLBP;
			varLBP += diff*diff;
		}
		
		varLBP = sqrt(varLBP);
		varLBP /= 9.0;
		
		if (varLBP < 4.0 &&
			(lbpFeats[2] == minLBP ||
			lbpFeats[2] == secMinLBP ) &&
			lbpFeats[6] < meanLBP &&
			lbpFeats[1] > meanLBP &&
			lbpFeats[7] > meanLBP ) {
			// Valid text region
			if (pureDetect) {
				int area = m_textROI[l].width*m_textROI[l].height;
				if (area > maxRegionSize) {
					curRegionIdx = l;
					maxRegionSize = area;
				}
			}
			else {
				ulc1.x = m_textROI[l].x;
				ulc1.y = m_textROI[l].y;
				lrc1.x = ulc1.x + m_textROI[l].width;
				lrc1.y = ulc1.y + m_textROI[l].height;
				
				if (trackedXCen > ulc1.x && trackedXCen < lrc1.x &&
					trackedYCen > ulc1.y && trackedYCen < lrc1.y) {
					// Found a face in given ROI
					curRegionIdx = l;
					break;
				}
			}
		}
	}

	
	//set background (white)
	memset(m_binarized->imageData,0xFF,m_width * m_height);

	if (curRegionIdx != -1) {
		int hsx = m_textROI[curRegionIdx].width/2;
		int hsy = m_textROI[curRegionIdx].height/2;
		m_outROI->SetXCen(m_textROI[curRegionIdx].x + hsx);
		m_outROI->SetYCen(m_textROI[curRegionIdx].y + hsy);
		m_outROI->Reset(hsx, hsy, 0);
	
	
		//binarize image
		processRect(m_textROI[curRegionIdx], m_src, NULL, m_maxStd, m_minDist, &threshold);
	
		//set region of interest
		cvSetImageROI(m_gray,m_textROI[curRegionIdx]);
		cvSetImageROI(m_binarized,m_textROI[curRegionIdx]);
		
		//binarize
		cvThreshold(m_gray,m_binarized,threshold,255,CV_THRESH_BINARY);
	
	
		(*outputs[m_roiOutID].buffer)[count] = m_outROI;
	}
	else {
		(*outputs[m_roiOutID].buffer)[count] = nilObject;
	}
	
	// End timer
	//ftime(&t2);
			
	// Display time used
	//double timeDiff=(t2.time-t1.time)+((t2.millitm-t1.millitm)/1000.0);
	//cout << "Total run time (sec): " << timeDiff << endl;
			
	//
	// Produce output image
	//

	// Allocate output image
	
	Image *outImg = Image::alloc(m_width, m_height, 1);

	// Copy image pixels
	memcpy(outImg->get_data(), m_binarized->imageData, m_numPixels);
	// Produce output reference
	(*outputs[m_imageOutID].buffer)[count] = ObjectRef(outImg);
	
	// Output edges orientations
	(*outputs[m_edgesOriOutID].buffer)[count] = m_edgesOriSumRef;

      }
      catch (BaseException *e) {
	throw e->add(new GeneralException("Exception in TextSignDetect::calculate:",__FILE__,__LINE__));
      }
    }
	
    bool processRect(CvRect rect, IplImage *src, IplImage *dest, double std_max, double min_dist, double *threshold = 0) 
    {
      //Set Region of interest
      cvSetImageROI(src,rect);
      cvSetImageROI(m_mask,rect);
      bool bimodal = false;
      int x1,y1,x2,y2;
      x1 = rect.x;
      y1 = rect.y;
      x2 = rect.x + rect.width;
      y2 = rect.y + rect.height;
		
      CvScalar mean0,mean1,mean2;
      CvScalar stddev0,stddev1,stddev2;
		
      //Get Mean, std dev
      cvAvgSdv(src,&mean0,&stddev0);
      //cerr<<"CV made (mean0): "<<mean0.val[0]<<" "<<mean0.val[1]<<" "<<mean0.val[2]<<endl;
      //cerr<<"CV made (stddev0): "<<stddev0.val[0]<<" "<<stddev0.val[1]<<" "<<stddev0.val[2]<<endl;
      //cerr<<"JM TEST "<<sqrt(stddev0.val[0] * stddev0.val[0] + 
      //			     stddev0.val[1] * stddev0.val[1] + 
      //		     stddev0.val[2] * stddev0.val[2])<<endl;
	
      //LOOK FOR STDDEV1 IF WE SHOULD CONTINUE...
		
      //BRIGHTER
      mean1.val[0] = min(mean0.val[0] + 10,255.0);
      mean1.val[1] = min(mean0.val[1] + 10,255.0);
      mean1.val[2] = min(mean0.val[2] + 10,255.0);
		
      //DARKER
      mean2.val[0] = max(mean0.val[0] - 10,0.0);
      mean2.val[1] = max(mean0.val[1] - 10,0.0);
      mean2.val[2] = max(mean0.val[2] - 10,0.0);
	
      int nb[2] = {0 ,0 };
		
      //calculate euclidian distance
      for (int row = y1; row < y2; row++) {
		
	char *basePtr = &m_src->imageData[(row * m_src->widthStep) + (rect.x * m_src->nChannels)];
	char *imgPtr = basePtr;
	char *maskPtr = &m_mask->imageData[(row * m_mask->widthStep) + (rect.x * m_mask->nChannels)];
	
	for (; imgPtr < basePtr + (m_src->nChannels *  rect.width); imgPtr += m_src->nChannels, maskPtr += m_mask->nChannels) {
		
	  //get image data
	  double Red = (unsigned char) imgPtr[0];
	  double Green = (unsigned char) imgPtr[1];
	  double Blue = (unsigned char) imgPtr[2];
				
	  double dist1 = (Red - mean1.val[0]) * (Red - mean1.val[0]) +
	    (Green - mean1.val[1]) * (Green - mean1.val[1]) +
	    (Blue - mean1.val[2]) * (Blue - mean1.val[2]);
	
	  double dist2 = (Red - mean2.val[0]) * (Red - mean2.val[0]) +
	    (Green - mean2.val[1]) * (Green - mean2.val[1]) +
	    (Blue - mean2.val[2]) * (Blue - mean2.val[2]);
	
	  if (dist1 <= dist2) {
	    //DIST1 SMALLER, SET MASK PROPERLY FOR MEAN FUNCTION TO BE CALCULATED PROPERLY
	    *maskPtr = 1;	    
	    nb[0]++;
	  }
	  else {
	    //DIST2 SMALLER, SET MASK PROPERLY FOR MEAN FUNCTION TO BE CALCULATED PROPERLY
	    *maskPtr = 0;
	    nb[1]++;
	  }
	}
      }
		
      //CALCULATE NEW AVG AND STD FOR NEWLY FOUND PIXELS
      cvAvgSdv(src,&mean1,&stddev1,m_mask);
	
		
      //INVERT MASK
      for (int row = y1; row < y2; row++) {	
	char *basePtr = &m_mask->imageData[(row * m_mask->widthStep) + (rect.x * m_mask->nChannels)];
	char *maskPtr = basePtr;
		
	for (; maskPtr < basePtr + (m_mask->nChannels *  rect.width); maskPtr += m_mask->nChannels) {
	  if (*maskPtr == 1) {
	    *maskPtr = 0;
	  }
	  else {
	    *maskPtr = 1;
	  }
	}
      }
		
      //TODO STDDEV ON NO ELEMENT ?
	
      //CALCULATE NEW AVG AND STD FOR NEWLY FOUND PIXELS
      cvAvgSdv(src,&mean2,&stddev2,m_mask);
	
      //cerr<<"CV made (mean1): "<<mean1.val[0]<<" "<<mean1.val[1]<<" "<<mean1.val[2]<<endl;
      //cerr<<"CV made (stddev1): "<<stddev1.val[0]<<" "<<stddev1.val[1]<<" "<<stddev1.val[2]<<endl;
      //cerr<<"JM TEST "<<sqrt(stddev1.val[0] * stddev1.val[0] + 
      //			     stddev1.val[1] * stddev1.val[1] + 
      //			     stddev1.val[2] * stddev1.val[2])<<endl;
	
      //cerr<<"CV made (mean2): "<<mean2.val[0]<<" "<<mean2.val[1]<<" "<<mean2.val[2]<<endl;
      //cerr<<"CV made (stddev2): "<<stddev2.val[0]<<" "<<stddev2.val[1]<<" "<<stddev2.val[2]<<endl;
      //cerr<<"JM TEST "<<sqrt(stddev2.val[0] * stddev2.val[0] + 
      //		     stddev2.val[1] * stddev2.val[1] + 
      //		     stddev2.val[2] * stddev2.val[2])<<endl;
	
      //cerr<<"nbsample " <<nb[0]<<" "<<nb[1]<<endl;
	
      //DISTANCE ENTRE LES 2 MOYENNES SOIT SUFFISANTE
      //STD SOIT PETIT PAR RAPPORT A LA MOYENNE
      double mean_dist = sqrt((mean1.val[0] - mean2.val[0]) * (mean1.val[0] - mean2.val[0]) +
			      (mean1.val[1] - mean2.val[1]) * (mean1.val[1] - mean2.val[1]) +
			      (mean1.val[2] - mean2.val[2]) * (mean1.val[2] - mean2.val[2]));
	
      double std1sum = sqrt(stddev1.val[0] * stddev1.val[0]+ 
			    stddev1.val[1] * stddev1.val[1] + 
			    stddev1.val[2] * stddev1.val[2]);
	
      double std2sum = sqrt(stddev2.val[0] * stddev2.val[0]+ 
			    stddev2.val[1] * stddev2.val[1] + 
			    stddev2.val[2] * stddev2.val[2]);
	
      //BINARIZATION THRESHOLD CALCULATION (IF REQUIRED)
      if (threshold) {
	*threshold = (mean1.val[0] + mean2.val[0] + mean1.val[1] + mean2.val[1] + mean1.val[2] + mean2.val[2]) / 6.0; 
      }
	
      if (mean_dist > min_dist /*&&
				 std1sum < std_max &&
				 std2sum < std_max */ ) {
	bimodal = true;
      }
		
      cvResetImageROI(m_src);
      cvResetImageROI(m_mask);
      cvResetImageROI(m_gray);
      cvResetImageROI(m_edges);
	
      return bimodal;

    }

  private:
    // Node inputs 
    int m_imageInID;
	int m_roiInID;
    int m_activationInID;
	
    // Node outputs
    int m_imageOutID;
    int m_roiOutID;
	int m_edgesOriOutID;

    // Image parameters
    int m_width;
    int m_height;
    int m_numChannels;
    int m_numPixels;
    int m_numBytesPerFrame;
	
    // Method parameters
    int m_numOriBins;
    float m_edgesStrTresh;
    double m_minDist;
    double m_maxStd;
    double m_horiSymThresh;
    double m_vertSymThresh;

    // Internal images
    IplImage *m_src;
    IplImage *m_gray;
    IplImage *m_mask;
    IplImage *m_edges;
    IplImage *m_binarized;
	IplImage *m_tmpBinarized;
    IplImage *m_oriXImage;
    IplImage *m_oriYImage;
    IplImage **m_edgesOri;
    IplImage **m_edgesOriSum;
	
    // Pixel/value pointers
    float **m_edgesOriPix;
    double *m_tmpOriVal;
    double **m_edgesOriSumPix;
	
    // Potential text region of interest parameters
    int m_textROIMinWidth;
    int m_textROIMinHeight;
    int m_textROIMaxWidth;
    int m_textROIMaxHeight;
    float m_edgesOriMaxBinsStd;
    int m_maxNumTextROI;
    // Potential text region of interest stored in this structure
    CvRect *m_textROI;
    CvRect *m_finalTextROI;
    int *m_labels;
	
    // Internal contour memory storage
    CvMemStorage *m_contourStorage;
	
    RCPtr<VisualROI> m_outROI;
	RCPtr<Vector<double *> > m_edgesOriSumRef;
	
	IntegralLBPExtraction *m_intLBPExtract;
  };

}

#endif
