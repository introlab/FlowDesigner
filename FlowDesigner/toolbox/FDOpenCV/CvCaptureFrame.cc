#ifndef _CVCAPTUREFRAME_CC_
#define _CVCAPTUREFRAME_CC_

#include "BufferedNode.h"

#include "cv.h"
#include "highgui.h"

#include "Image.h"
#include "VisualROI.h"
#include <stdlib.h>
#include <sys/timeb.h>

using namespace FD;
using namespace std;

namespace RobotFlow {

class CvCaptureFrame;

DECLARE_NODE(CvCaptureFrame)

  /*Node
   *
   * @name CvCaptureFrame
   * @category RobotFlow:Vision:Detection
   * @description Capture a frame using OpenCV.
   *
   * @parameter_name FRAME_WIDTH
   * @parameter_type int
   * @parameter_value 640
   * @parameter_description Video frame width.
   *
   * @parameter_name FRAME_HEIGHT
   * @parameter_type int
   * @parameter_value 480
   * @parameter_description Video frame height.
   *
   * @parameter_name DEVICE
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Device where to grab the frame (/dev/video#).
   *
   * @output_name OUT_IMAGE
   * @output_type Image
   * @output_description Grabbed image.
   *
   END*/

class CvCaptureFrame : public BufferedNode {

public:

	CvCaptureFrame(string nodeName, ParameterSet params)
	: BufferedNode(nodeName, params)
	, m_capture(NULL)
	, m_frame(NULL)
	{
		m_imageOutID = addOutput("OUT_IMAGE");

		m_width = dereference_cast<int>(parameters.get("FRAME_WIDTH"));
		m_height = dereference_cast<int>(parameters.get("FRAME_HEIGHT"));
		m_device = dereference_cast<int>(parameters.get("DEVICE"));
		
		m_capture = cvCaptureFromCAM( m_device );
		cvSetCaptureProperty( m_capture, CV_CAP_PROP_FRAME_WIDTH, (double)m_width );
		cvSetCaptureProperty( m_capture, CV_CAP_PROP_FRAME_HEIGHT, (double)m_height );
	}

	virtual ~CvCaptureFrame()
	{
		cerr<<"CvCaptureFrame destruction"<<endl;
		cvReleaseCapture( &m_capture );
	}
	
	void calculate(int output_id, int count, Buffer &out)
	{
		try {
			// Get activation flag 
			//m_activated = getInput(m_activatedInID, count);
			
			//if (!(*m_activated)) {
			//	// Output nilObjects and return
			//	(*outputs[m_roiOutID].buffer)[count] = nilObject;
			//	return;
			//}
			
			if( m_capture )
			{
				if( !cvGrabFrame( m_capture ))
					return;
				m_frame = cvRetrieveFrame( m_capture );
				if( !m_frame )
					return;
				
				//fprintf( stdout, "Image capture's type = %s %s %i\n", m_frame->colorModel, m_frame->channelSeq, m_frame->depth );
				//fprintf( stdout, "Image capture's size = %i %i %i\n", m_frame->width, m_frame->height, m_frame->nChannels );
				//fprintf(stdout, "BGR? %i\n", strcmp(m_frame->channelSeq, "BGR"));
				if(strcmp(m_frame->channelSeq, "BGR") == 0)
				{
					cvCvtColor(m_frame, m_frame, CV_BGR2RGB);
				}
					
				int m_pixelSize = m_frame->nChannels;

				Image *my_image = NULL;

				if (m_pixelSize == 2){
					my_image = Image::alloc(m_width, m_height, m_pixelSize);
					memcpy(my_image->get_data(),m_frame->imageData, m_frame->imageSize);
					out[count] = ObjectRef(my_image);
				}
				else if (m_pixelSize == 3) {
					my_image = Image::alloc(m_width, m_height, m_pixelSize);
					memcpy(my_image->get_data(),m_frame->imageData, m_frame->imageSize);
					out[count] = ObjectRef(my_image);
				}
				else {
					throw new GeneralException("PIXELSIZE not yet supported",__FILE__,__LINE__);
				}
			
				//FIXME: It crached when tring to release m_frame ?
				//cvReleaseImage(&m_frame);
				//m_frame = NULL;
			}
		}
		catch (BaseException *e) {
	        	throw e->add(new GeneralException("Exception in CvCaptureFrame::calculate:",__FILE__,__LINE__));
	        }
	}

private:
	int m_imageOutID;
	
	CvCapture* m_capture;
	IplImage * m_frame;
	
	int m_device;	
	int m_width;
	int m_height;
	
  //CvMemStorage *m_storage;
	
	
};

}

#endif
