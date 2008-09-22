#ifndef _CVFACEDETECTION_CC_
#define _CVFACEDETECTION_CC_

#include "BufferedNode.h"
#include "cv.h"
#include "cvaux.h"
#include "Image.h"
#include "VisualROI.h"
#include <stdlib.h>
#include <sys/timeb.h>

using namespace FD;
using namespace std;

namespace RobotFlow {

class CvFaceDetection;

DECLARE_NODE(CvFaceDetection)

  /*Node
   *
   * @name CvFaceDetection
   * @category RobotFlow:Vision:Detection
   * @description Haar cascade classifier applied to face recognition using OpenCV.
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
   * @parameter_name MAX_NUM_SKIN_REGIONS
   * @parameter_type int
   * @parameter_value 20
   * @parameter_description Maximum number of skin regions to search for faces in a frame.
   *
   * @parameter_name MIN_SKIN_ROI_WIDTH
   * @parameter_type int
   * @parameter_value 24
   * @parameter_description Minimum width of a skin region to consider for face detection.
   *
   * @parameter_name MIN_SKIN_ROI_HEIGHT
   * @parameter_type int
   * @parameter_value 24
   * @parameter_description Minimum height of a skin region to consider for face detection.
   *
   * @input_name IN_IMAGE
   * @input_type Image
   * @input_description Current grayscale frame to process.
   *
   * @input_name IN_SKIN_MASK
   * @input_type Image
   * @input_description Current skin color segmentation result.
   *
   * @input_name IN_TRACKED_ROI
   * @input_type VisualROI
   * @input_description Region of interest currently tracked.
   *
   * @input_name ACTIVATION
   * @input_type bool
   * @input_description Node activation flag.
   *
   * @output_name OUT_ROI
   * @output_type VisualROI
   * @output_description Region of interest corresponding to current model.
   *
   END*/

class CvFaceDetection : public BufferedNode {

public:

	CvFaceDetection(string nodeName, ParameterSet params)
	: BufferedNode(nodeName, params), m_storage1(NULL), m_storage2(NULL),
	m_haarClassifier1(NULL), m_haarClassifier2(NULL)
	{
		m_imageInID = addInput("IN_IMAGE");
		m_maskInID = addInput("IN_SKIN_MASK");
		m_roiInID = addInput("IN_TRACKED_ROI");
		m_activatedInID = addInput("ACTIVATION");
		
		m_roiOutID = addOutput("OUT_ROI");

		m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
		m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
		m_maxNumSkinRegions = dereference_cast<int>(parameters.get("MAX_NUM_SKIN_REGIONS"));
		m_minROIWidth = dereference_cast<int>(parameters.get("MIN_SKIN_ROI_WIDTH"));
		m_minROIHeight =dereference_cast<int>(parameters.get("MIN_SKIN_ROI_HEIGHT"));
		m_numChannels = 3;
		m_numPixels = m_width*m_height;
		m_numBytesInFrame = m_numPixels*m_numChannels;
		m_imgXCen = m_width/2;
		m_imgYCen = m_height/2;
		
		CvSize imgSize;
		imgSize.width = m_width;
		imgSize.height = m_height;
		
		m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
		m_skinMask = cvCreateImage(imgSize, IPL_DEPTH_8U, 1);
		m_filtMask = cvCreateImage(imgSize, IPL_DEPTH_8U, 1);
		
		m_storage1 = cvCreateMemStorage(0);
		m_storage2 = cvCreateMemStorage(0);
		
		m_haarClassifier1 = (CvHaarClassifierCascade*)cvLoad( "haarcascade_frontalface_alt2.xml", 0, 0, 0 );
		m_haarClassifier2 = (CvHaarClassifierCascade*)cvLoad( "haarcascade_profileface.xml", 0, 0, 0 );
		
		if (!m_haarClassifier1 || !m_haarClassifier2) {
		  throw new GeneralException("Cannot load haar classifiers : from files haarcascade_frontalface_alt2.xml, haarcascade_profileface.xml",__FILE__,__LINE__);
		}

		m_curSkinROI = new CvRect[m_maxNumSkinRegions];
		
		m_contourStorage = cvCreateMemStorage(0);
		
		m_faceROI = RCPtr<VisualROI>(new VisualROI(e_VISUALROI_rectangular, 1, 1, 1, 1, 0));
	}
	
	CvFaceDetection()
	{
		delete [] m_curSkinROI;
		cvReleaseImage(&m_filtMask);
		cvReleaseImage(&m_skinMask);
		cvReleaseImage(&m_curImage);
	}

	void calculate(int output_id, int count, Buffer &out)
	{
		try {
			// Get activation flag 
			m_activated = getInput(m_activatedInID, count);
			
			if (!(*m_activated)) {
				// Output nilObjects and return
				(*outputs[m_roiOutID].buffer)[count] = nilObject;
				return;
			}
			
			
			//struct timeb t1, t2;
			bool foundFace = false;
			RCPtr<Image> imageRef = getInput(m_imageInID, count);
			
			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("CvFaceDetection::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Copy input image
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
			
			// Start timer
			//ftime(&t1);
			
			// Get IN_TRACKED_ROI
			ObjectRef roiRef = getInput(m_roiInID, count);
				
			if (roiRef->isNil()) {
				// No tracked target, so do standard detection
				
				// Try to get a skin mask
				ObjectRef maskRef = getInput(m_maskInID, count);
					
				if (!maskRef->isNil()) {
					// Initialize target at current ROI
					RCPtr<Image> skinMaskImgRef = maskRef;
					
					// Verify input image sanity
					if (skinMaskImgRef->get_width() != m_width ||
						skinMaskImgRef->get_height() != m_height ||
						skinMaskImgRef->get_pixelsize() != 1) {
						throw new GeneralException ("CvFaceDetection::calculate : skin mask image parameters do not correspond to given input.",__FILE__,__LINE__);
					}
					
					// Copy input image
					memcpy(m_skinMask->imageData, skinMaskImgRef->get_data(), m_numPixels);
					
					foundFace = FaceDetectionWSkin();
				}
				else {
					foundFace = FaceDetectionStd();
				}
			}
			else {
				// Need to validate tracked ROI
				RCPtr<VisualROI> roiRefPtr = roiRef;
				
				foundFace = FaceDetectionWROI(roiRefPtr.get());
			}	
			// End timer
			//ftime(&t2);
			
			// Display time used
			//double timeDiff=(t2.time-t1.time)+((t2.millitm-t1.millitm)/1000.0);
			//cout << "Total run time (sec): " << timeDiff << " found face=" << foundFace << endl;
			
			
			if (foundFace) {
				(*outputs[m_roiOutID].buffer)[count] = m_faceROI;
			}
			else {
				(*outputs[m_roiOutID].buffer)[count] = nilObject;
			}
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in CvFaceDetection::calculate:",__FILE__,__LINE__));
		}
	}
	
	bool FaceDetectionStd() 
	{
		try {
			cvClearMemStorage(m_storage1);
				
			// Detect upperbodies
			CvSeq* faces = cvHaarDetectObjects( m_curImage, m_haarClassifier1, m_storage1,
				1.2, 2, CV_HAAR_DO_CANNY_PRUNING, cvSize(0,0) );
				
			cvClearMemStorage(m_storage2);
			
			CvSeq* profiles = cvHaarDetectObjects( m_curImage, m_haarClassifier2, m_storage2,
				1.2, 2, CV_HAAR_DO_CANNY_PRUNING, cvSize(0,0) );
			
			float bestDist = sqrt((float)(m_width*m_width + m_height*m_height));
			int curXCen = 0;
			int curYCen = 0;
			int curHSX = 0;
			int curHSY = 0;
			int i, hsX, hsY, xCen, yCen, xDist, yDist;
			float curDist;
			CvPoint pt1, pt2;
			
			// Display identified objects
			for(i = 0; i < (faces ? faces->total : 0); i++ ) {
				CvRect* r = (CvRect*)cvGetSeqElem( faces, i );
				pt1.x = r->x;
				pt2.x = (r->x+r->width);
				pt1.y = r->y;
				pt2.y = (r->y+r->height);
				
				hsX = r->width/2;
				hsY = r->height/2;
				xCen = r->x + hsX;
				yCen = r->y + hsY;
				hsX = (int)((float)(hsX)*0.7f);
				xDist = xCen-m_imgXCen;
				yDist = yCen-m_imgYCen;
				curDist = sqrt((float)(xDist*xDist + yDist*yDist));
				
				if (curDist < bestDist) {
					bestDist = curDist;
					curXCen = xCen;
					curYCen = yCen;
					curHSX = hsX;
					curHSY = hsY;
					isFrontFace = true;
					isProfileFace = false;
				}
			}
			
			for(i = 0; i < (profiles ? profiles->total : 0); i++ ) {
				CvRect* r = (CvRect*)cvGetSeqElem( profiles, i );
				pt1.x = r->x;
				pt2.x = (r->x+r->width);
				pt1.y = r->y;
				pt2.y = (r->y+r->height);
				
				hsX = r->width/2;
				hsY = r->height/2;
				xCen = r->x + hsX;
				yCen = r->y + hsY;
				hsX = (int)((float)(hsX)*0.6f);
				hsY = (int)((float)(hsY)*0.9f);
				xDist = xCen-m_imgXCen;
				yDist = yCen-m_imgYCen;
				curDist = sqrt((float)(xDist*xDist + yDist*yDist));
				
				if (curDist < bestDist) {
					bestDist = curDist;
					curXCen = xCen;
					curYCen = yCen;
					curHSX = hsX;
					curHSY = hsY;
					isFrontFace = false;
					isProfileFace = true;
				}
			}
			
			if (curHSX > 0 && curHSY > 0) {
				m_faceROI->SetXCen(curXCen);
				m_faceROI->SetYCen(curYCen);
				m_faceROI->Reset(curHSX, curHSY, 0);
				
				return true;
			}
			else {
				return false;
			}
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in CvFaceDetection::FaceDetectionStd:",__FILE__,__LINE__));
		}
	}
	
	bool FaceDetectionWSkin()
	{
		try {
			// First identify skin blobs that could be appropriate for faces
			int r;
			int curNumSkinRegions = 0;
			float bestDist = sqrt((float)(m_width*m_width + m_height*m_height));
			int curXCen = 0;
			int curYCen = 0;
			int curHSX = 0;
			int curHSY = 0;
			int i, hsX, hsY, xCen, yCen, xDist, yDist;
			float curDist;
			CvPoint pt1, pt2;
			// Contour (connected compnents) sequence
			CvSeq *contour;
			// Pointer to skin regions parameters
			CvRect *p_regions = m_curSkinROI;
			
			// Smooth mask
			cvSmooth(m_skinMask, m_filtMask, CV_MEDIAN, 3, 0);
			
			// Find connected components contours using a simple chain code approximation
			cvFindContours(m_filtMask, m_contourStorage, &contour, sizeof(CvContour), CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE);
			
			// Process each contour to determine if it is a potential face region of interest
			for( ; contour != 0; contour = contour->h_next ) {
				// Get rectangular bounding box of current contour
				CvRect bbox = cvContourBoundingRect(contour, 1);
				// Accept only regions fitting the desired scale
				if (bbox.width > m_minROIWidth && 
					bbox.height > m_minROIHeight) {
					if (curNumSkinRegions < m_maxNumSkinRegions) {
						// Copy current region information
						// but increase its size to be sure to fit all the head
						(*p_regions).x = bbox.x - (int)((float)(bbox.width)*0.2f);
						if ((*p_regions).x < 0) {
							(*p_regions).x = 0;
						}
						
						(*p_regions).y = bbox.y - (int)((float)(bbox.height)*0.1f);
						if ((*p_regions).y < 0) {
							(*p_regions).y = 0;
						}
						
						(*p_regions).width = (int)((float)(bbox.width)*1.4f);
						(*p_regions).height = (int)((float)(bbox.height)*1.2f);
						
						curNumSkinRegions++;
						p_regions++;
					}
					else {
						// No more storage, warn and continue to next step
						cerr << "CvFaceDetection::FaceDetectionWSkin: WARNING: could not process all potential skin regions because of the given limit of " << m_maxNumSkinRegions << " regions." << endl;
					}
				}
			}
			
			for (r=0; r<curNumSkinRegions; r++) {
				// Apply Haar cascade face detection only on selected ROIs
				// Set current image ROI to the corresponding skin region
				cvSetImageROI(m_curImage, m_curSkinROI[r]);
				
				// Since we are setting ROI with the OpenCV image
				// we need to add an offset to the returned position
				int xOffset = m_curSkinROI[r].x;
				int yOffset = m_curSkinROI[r].y;
				
				cvClearMemStorage(m_storage1);
					
				// Detect frontal faces
				CvSeq* faces = cvHaarDetectObjects( m_curImage, m_haarClassifier1, m_storage1,
					1.2, 2, CV_HAAR_DO_CANNY_PRUNING, cvSize(0,0) );
					
				cvClearMemStorage(m_storage2);
				
				// Detect face profiles
				CvSeq* profiles = cvHaarDetectObjects( m_curImage, m_haarClassifier2, m_storage2,
					1.2, 2, CV_HAAR_DO_CANNY_PRUNING, cvSize(0,0) );
				
				// Display identified objects
				for(int i = 0; i < (faces ? faces->total : 0); i++ ) {
					CvRect* r = (CvRect*)cvGetSeqElem( faces, i );
					pt1.x = r->x;
					pt2.x = (r->x+r->width);
					pt1.y = r->y;
					pt2.y = (r->y+r->height);
					
					hsX = r->width/2;
					hsY = r->height/2;
					// Since we are setting ROI with the OpenCV image
					// we need to add an offset to the returned position
					xCen = r->x + hsX + xOffset;
					yCen = r->y + hsY + yOffset;
					hsX = (int)((float)(hsX)*0.7f);
					xDist = xCen-m_imgXCen;
					yDist = yCen-m_imgYCen;
					curDist = sqrt((float)(xDist*xDist + yDist*yDist));
					
					if (curDist < bestDist) {
						bestDist = curDist;
						curXCen = xCen;
						curYCen = yCen;
						curHSX = hsX;
						curHSY = hsY;
						isFrontFace = true;
						isProfileFace = false;
					}
				}
				
				for(int i = 0; i < (profiles ? profiles->total : 0); i++ ) {
					CvRect* r = (CvRect*)cvGetSeqElem( profiles, i );
					pt1.x = r->x;
					pt2.x = (r->x+r->width);
					pt1.y = r->y;
					pt2.y = (r->y+r->height);
					
					hsX = r->width/2;
					hsY = r->height/2;
					// Since we are setting ROI with the OpenCV image
					// we need to add an offset to the returned position
					xCen = r->x + hsX + xOffset;
					yCen = r->y + hsY + yOffset;
					hsX = (int)((float)(hsX)*0.6f);
					hsY = (int)((float)(hsY)*0.9f);
					xDist = xCen-m_imgXCen;
					yDist = yCen-m_imgYCen;
					curDist = sqrt((float)(xDist*xDist + yDist*yDist));
					
					if (curDist < bestDist) {
						bestDist = curDist;
						curXCen = xCen;
						curYCen = yCen;
						curHSX = hsX;
						curHSY = hsY;
						isFrontFace = false;
						isProfileFace = true;
					}
				}
			}
			
			// Clear contour information
			cvClearMemStorage(m_contourStorage);
			
			// Reset current image ROI
			cvSetImageROI(m_curImage, cvRect(0, 0, m_width, m_height));
			
			if (curHSX > 0 && curHSY > 0) {
				m_faceROI->SetXCen(curXCen);
				m_faceROI->SetYCen(curYCen);
				m_faceROI->Reset(curHSX, curHSY, 0);
				
				return true;
			}
			else {
				return false;
			}
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in CvFaceDetection::FaceDetectionStd:",__FILE__,__LINE__));
		}
	}
	
	bool FaceDetectionWROI(VisualROI *i_trackedROI) 
	{
		try {
			int trackedXCen = i_trackedROI->GetXCen();
			int trackedYCen = i_trackedROI->GetYCen();
			
			cvClearMemStorage(m_storage1);
				
			// Detect upperbodies
			CvSeq* faces = cvHaarDetectObjects( m_curImage, m_haarClassifier1, m_storage1,
				1.2, 2, CV_HAAR_DO_CANNY_PRUNING, cvSize(0,0) );
				
			cvClearMemStorage(m_storage2);
			
			CvSeq* profiles = cvHaarDetectObjects( m_curImage, m_haarClassifier2, m_storage2,
				1.2, 2, CV_HAAR_DO_CANNY_PRUNING, cvSize(0,0) );
			
			int curXCen = 0;
			int curYCen = 0;
			int curHSX = 0;
			int curHSY = 0;
			int i;
			CvPoint pt1, pt2;
			
			// Find a face near the given ROI
			for(i = 0; i < (faces ? faces->total : 0); i++ ) {
				CvRect* r = (CvRect*)cvGetSeqElem( faces, i );
				pt1.x = r->x;
				pt2.x = (r->x+r->width);
				pt1.y = r->y;
				pt2.y = (r->y+r->height);
				
				if (trackedXCen > pt1.x && trackedXCen < pt2.x &&
					trackedYCen > pt1.y && trackedYCen < pt2.y) {
					// Found a face in given ROI
					curHSX = r->width/2;
					curHSY = r->height/2;
					curXCen = r->x + curHSX;
					curYCen = r->y + curHSY;
					m_faceROI->SetXCen(curXCen);
					m_faceROI->SetYCen(curYCen);
					m_faceROI->Reset(curHSX, curHSY, 0);
					isFrontFace = true;
					isProfileFace = false;
					
					return true;
				}
			}
			
			for(i = 0; i < (profiles ? profiles->total : 0); i++ ) {
				CvRect* r = (CvRect*)cvGetSeqElem( profiles, i );
				pt1.x = r->x;
				pt2.x = (r->x+r->width);
				pt1.y = r->y;
				pt2.y = (r->y+r->height);
				
				if (trackedXCen > pt1.x && trackedXCen < pt2.x &&
					trackedYCen > pt1.y && trackedYCen < pt2.y) {
					// Found a face in given ROI
					curHSX = r->width/2;
					curHSY = r->height/2;
					curXCen = r->x + curHSX;
					curYCen = r->y + curHSY;
					m_faceROI->SetXCen(curXCen);
					m_faceROI->SetYCen(curYCen);
					m_faceROI->Reset(curHSX, curHSY, 0);
					isFrontFace = false;
					isProfileFace = true;
					
					return true;
				}
			}
			
			// If we get here, there were no face detected near the given ROI
			return false;
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in CvFaceDetection::FaceDetectionStd:",__FILE__,__LINE__));
		}
	}

private:
	int m_imageInID;
	int m_maskInID;
	int m_roiInID;
	int m_activatedInID;
	int m_roiOutID;
	
	RCPtr<Bool> m_activated;
	
	int m_width;
	int m_height;
	int m_numChannels;
	int m_numPixels;
	int m_numBytesInFrame;
	int m_imgXCen;
	int m_imgYCen;
	
	int m_maxNumSkinRegions;
	int m_minROIWidth;
	int m_minROIHeight;
	
	CvMemStorage *m_storage1;
	CvMemStorage *m_storage2;
	CvHaarClassifierCascade *m_haarClassifier1;
	CvHaarClassifierCascade *m_haarClassifier2;
	
	CvRect *m_curSkinROI;
	
	IplImage *m_curImage;
	IplImage *m_skinMask;
	IplImage *m_filtMask;
	
	// Internal contour memory storage
	CvMemStorage *m_contourStorage;
	
	RCPtr<VisualROI> m_faceROI;
	bool isFrontFace;
	bool isProfileFace;
};

}

#endif
