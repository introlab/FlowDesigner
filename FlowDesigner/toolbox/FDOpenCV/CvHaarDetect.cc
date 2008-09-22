/*
 * performance.cpp
 *
 * Measure performance of classifier
 */
#include "BufferedNode.h"
#include "cv.h"
#include "VisualROI.h"
#include "Image.h"
#include <stdio.h>
#include <math.h>
#include <highgui.h>
#include <time.h>
#include <Vector.h>

using namespace FD;
using namespace std;

#ifdef _WIN32
/* use clock() function insted of time() */
#define time( arg ) (((double) clock()) / CLOCKS_PER_SEC)
#endif /* _WIN32 */

#ifndef PATH_MAX
#define PATH_MAX 512
#endif /* PATH_MAX */

namespace RobotFlow {

typedef struct HidCascade
{
    int size;
    int count;
} HidCascade;

typedef struct ObjectPos
{
    float x;
    float y;
    float width;
    int found;    /* for reference */
    int neghbors;
} ObjectPos;

class CvHaarDetect;

DECLARE_NODE(CvHaarDetect)

  /*Node
   *
   * @name CvHaarDetect
   * @category RobotFlow:Vision:Detection
   * @description Generic haar cascade classifier for recognition using OpenCV.
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
   * @parameter_description Number of channels of the input image.
   *
   * @parameter_name SCALE_FACTOR
   * @parameter_type float
   * @parameter_value 1.2
   * @parameter_description Detection parameter.
   *
   * @parameter_name CLASSIFIER
   * @parameter_type string
   * @parameter_value /home/npaquin/u2s/Sources/Vision/Haar/train10/prise10.xml
   * @parameter_description .xml file of the classifier.
   *
   * @input_name IN_IMAGE
   * @input_type Image
   * @input_description Current grayscale frame to process.
   *
   * @input_name ACTIVATION
   * @input_type bool
   * @input_description Node activation flag.
   *
   * @output_name OUT_ROI
   * @output_type Vector<ObjectRef>
   * @output_description Region of interest corresponding to current model.
   *
   * @output_name OUT_IMAGE
   * @output_type Image
   * @output_description Image with the ROI on it.
   *
   END*/

class CvHaarDetect : public BufferedNode {

public:

	CvHaarDetect(string nodeName, ParameterSet params)
	: BufferedNode(nodeName, params)
	, m_storage1(NULL)
	, m_haarClassifier1(NULL)
	{
		m_imageInID = addInput("IN_IMAGE");
		m_activatedInID = addInput("ACTIVATION");
		
		m_roiOutID = addOutput("OUT_ROI");
		m_ImageOutID = addOutput("OUT_IMAGE");

		m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
		m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
		m_numChannels = dereference_cast<int>(parameters.get("NUM_CHANNELS"));
		m_scaleFactor = dereference_cast<float>(parameters.get("SCALE_FACTOR"));
		m_classifier = object_cast<String>(parameters.get("CLASSIFIER"));
		m_numPixels = m_width*m_height;
		m_numBytesInFrame = m_numPixels*m_numChannels;
		m_imgXCen = m_width/2;
		m_imgYCen = m_height/2;
		
		CvSize imgSize;
		imgSize.width = m_width;
		imgSize.height = m_height;
		
		m_curImage = cvCreateImage(imgSize, IPL_DEPTH_8U, m_numChannels);
		
		m_storage1 = cvCreateMemStorage(0);
		
		// Read the .xml file representing the classifier constructed from the classifier dir with convert_cascade sample program in OpenCV
		m_haarClassifier1 = (CvHaarClassifierCascade*)cvLoad( m_classifier.val().c_str(), 0, 0, 0 );
		if (!m_haarClassifier1) {
		  throw new GeneralException("Cannot load haar classifiers : from file CLASSIFIER",__FILE__,__LINE__);
		}
		
		/*
		// cvLoadHaarClassifierCascade load the dir containing the clissifier (obsolete)
		m_haarClassifier1 = cvLoadHaarClassifierCascade( m_classifier.val().c_str(), cvSize( m_windowWidth, m_windowHeight ) );
		if( m_haarClassifier1 == NULL )
		{
			throw new GeneralException( "Unable to load classifier from file classifierdir",__FILE__,__LINE__);
		}
		*/

		/*
		int* numclassifiers = new int[m_haarClassifier1->count];
		numclassifiers[0] = m_haarClassifier1->stage_classifier[0].count;
		for( i = 1; i < m_haarClassifier1->count; i++ )
		{
			numclassifiers[i] = numclassifiers[i-1] + m_haarClassifier1->stage_classifier[i].count;
		}*/

		m_contourStorage = cvCreateMemStorage(0);
		
		m_ROIVect = RCPtr<Vector<ObjectRef> >(new Vector<ObjectRef>());
		  //(new VisualROI(e_VISUALROI_rectangular, 1, 1, 1, 1, 0));
	}
	
	virtual ~CvHaarDetect()
	{
		//cerr<<"CvHaarDetect destruction"<<endl;
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
				(*outputs[m_ImageOutID].buffer)[count] = nilObject;
				return;
			}
			
			//struct timeb t1, t2;
			bool foundObject = false;
			imageRef = getInput(m_imageInID, count);
			
			if(&imageRef == 0)
				throw new GeneralException ("CvHaarDetect::calculate : input image is null.",__FILE__,__LINE__);

			// Verify input image sanity
			if (imageRef->get_width() != m_width ||
				imageRef->get_height() != m_height ||
				imageRef->get_pixelsize() != m_numChannels) {
				throw new GeneralException ("CvHaarDetect::calculate : image parameters do not correspond to given input.",__FILE__,__LINE__);
			}
			
			// Copy input image
			memcpy(m_curImage->imageData, imageRef->get_data(), m_numBytesInFrame);
			
			// Start timer
			//ftime(&t1);
			
			//make sure we have an empty vector
			m_ROIVect->resize(0);

			// No tracked target, so do standard detection
			foundObject = ObjectDetection();

			// End timer
			//ftime(&t2);
			
			// Display time used
			//double timeDiff=(t2.time-t1.time)+((t2.millitm-t1.millitm)/1000.0);
			//cout << "Total run time (sec): " << timeDiff << " found object=" << foundObject << endl;
		
			unsigned char * temp = (unsigned char *) m_curImage->imageData;
			imageRef->put_data(temp, m_numBytesInFrame);
			(*outputs[m_ImageOutID].buffer)[count] = imageRef;
			
		
			(*outputs[m_roiOutID].buffer)[count] = m_ROIVect;
		
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in CvHaarDetect::calculate:",__FILE__,__LINE__));
		}
	}

	bool ObjectDetection() 
	{
		int i, detcount;
		ObjectPos* det;
		bool retval = false;

		try {
			cvClearMemStorage(m_storage1);
				
			// Detect objects
			CvSeq* objects = cvHaarDetectObjects( m_curImage, m_haarClassifier1, m_storage1, m_scaleFactor, 1 );
			
			detcount = ( objects ? objects->total : 0);
			det = (detcount > 0) ? ( (ObjectPos*)cvAlloc( detcount * sizeof( *det )) ) : NULL;

			for( i = 0; i < detcount; i++ ){
				CvAvgComp r = *((CvAvgComp*) cvGetSeqElem( objects, i ));
                		det[i].x = 0.5F * r.rect.width  + r.rect.x;
		                det[i].y = 0.5F * r.rect.height + r.rect.y;
		                det[i].width = sqrtf( 0.5F * (r.rect.width * r.rect.width +
		                                              r.rect.height * r.rect.height) );
	        	        det[i].neghbors = r.neighbors;
				
				cvRectangle( m_curImage, cvPoint( r.rect.x, r.rect.y ),
	                    		     cvPoint( r.rect.x + r.rect.width, r.rect.y + r.rect.height ),
		  	                     CV_RGB( 255, 0, 0 ), 3 );
				
				//std::cout << "ROI: x=" << r.rect.x << " y=" << r.rect.y 
				//	  << " width=" << r.rect.width << " height=" << r.rect.height 
				//	  << std::endl;	
				

				VisualROI *objectROI = new VisualROI(e_VISUALROI_rectangular, 1, 1, 1, 1, 0);

				objectROI->SetXCen(r.rect.x + r.rect.width / 2);
				objectROI->SetYCen(r.rect.y + r.rect.height / 2);
				objectROI->SetHSX(r.rect.width / 2);
				objectROI->SetHSY(r.rect.height / 2);
				
				m_ROIVect->push_back(ObjectRef(objectROI));

				retval = true;
		        }
		        
			if( det ) { cvFree( (void**)&det ); det = NULL; }			
			
		}
		catch (BaseException *e) {
			throw e->add(new GeneralException("Exception in CvHaarDetect::ObjectDetection:",__FILE__,__LINE__));
		}
		return retval;
	}

private:
	int m_imageInID;
	int m_activatedInID;
	int m_roiOutID;
	int m_ImageOutID;
	
	RCPtr<Bool> m_activated;
	
	int m_width;
	int m_height;
	float m_scaleFactor;
	String m_classifier;
	int m_numChannels;
	int m_numPixels;
	int m_numBytesInFrame;
	int m_imgXCen;
	int m_imgYCen;
	
	CvMemStorage *m_storage1;
	CvHaarClassifierCascade *m_haarClassifier1;
	
	IplImage *m_curImage;
	RCPtr<Image> imageRef;

	// Internal contour memory storage
	CvMemStorage *m_contourStorage;
	
        RCPtr<Vector<ObjectRef> > m_ROIVect;  
};

}

