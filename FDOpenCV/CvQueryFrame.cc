#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include "CvVideo.h"

using namespace std;

namespace FD {
   
   class CvQueryFrame;
   
   DECLARE_NODE(CvQueryFrame)
   /*Node
   *
   * @name CvQueryFrame
   * @category FDOpenCV:Video
   * @description Take a frame in the video
   *
   * @input_name VIDEO
   * @input_description Video capturing structure
   * @input_type CvVideo
   *
   * @output_name IMAGE
   * @output_description The result image
   * @output_type CvImage
   *
   * @output_name BOOL
   * @output_description True if the video isn't finish
   * @output_type bool 
   *
   END*/
   
   class CvQueryFrame : public BufferedNode {
      
      //Input ID
      int m_videoID;
      //Output ID
      int m_imageID;
      int m_boolID;
      
      public:
      CvQueryFrame(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_videoID = addInput("VIDEO");
         
         //add outputs
         m_imageID = addOutput("IMAGE"); 
         m_boolID = addOutput("BOOL");   
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvVideo> videoPtr = getInput(m_videoID,count);
         
         //Handle
         CvImage* image;   
         
         //Should not be released or modified
         IplImage* frame;
         bool Bool = true;
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL( frame = cvQueryFrame(videoPtr->getVideo()) );        
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to take a frame: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
         
         if(frame)
         {
            image = new CvImage(frame);            
         }
         else
         {
            Bool = false; 
            image = new CvImage();
         }
         
         (*(outputs[m_imageID].buffer))[count] = ObjectRef(image); 
         (*(outputs[m_boolID].buffer))[count] = ObjectRef(Bool::alloc(Bool));         
      }
      
      NO_ORDER_NODE_SPEEDUP(CvQueryFrame)
   };
}//namespace FD
