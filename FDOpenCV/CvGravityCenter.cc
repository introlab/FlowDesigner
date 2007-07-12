#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"
#include "CvImage.h"
#include "CvColor.h"

using namespace std;

namespace FD {
   
   class CvGravityCenter;
   
   DECLARE_NODE(CvGravityCenter)
   /*Node
   *
   * @name CvGravityCenter:Arithmetic Logic Comparison
   * @category FDOpenCV
   * @description Calculate the coordinate of the gravity center.
   *
   * @input_name IMAGE
   * @input_description The image to handle
   * @input_type CvImage
   *
   * @parameter_name BINARY
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description If the flag is non-zero, all the zero pixel values are treated as zeroes, all the others are treated as 0xFF.
   * 
   * @parameter_name CHANNEL
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Sets the channel of interest to a given value. Value 0 means that all channels are selected, 1 means that the first channel is selected etc.
   *
   * @output_name X
   * @output_description The coordinate x of the gravity center.
   * @output_type	float
   * 
   * @output_name Y
   * @output_description The coordinate y of the gravity center.
   * @output_type	float
   *
   END*/
   
   class CvGravityCenter : public BufferedNode {
      
      //Input ID
      int m_imageID;
      //Output ID
      int m_xID;
      int m_yID;
      //Parameters
      int m_binary;
      int m_channel;
      
      public:
      CvGravityCenter(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageID = addInput("IMAGE");
         
         //add outputs
         m_xID = addOutput("X");
         m_yID = addOutput("Y");
         
         //Initialize parameters         
         m_binary = dereference_cast<int>(parameters.get("BINARY"));
         m_channel = dereference_cast<int>(parameters.get("CHANNEL"));
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageID,count);  
         
         //Handle
         CvMoments* moments = new CvMoments();
         CvImage* image = new CvImage(imagePtr->getImage());
         float x=0,y=0;
         
         int status = cvGetErrMode();
         
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__;          
         OPENCV_CALL( cvSetImageCOI( image->getImage(), m_channel) );
         OPENCV_CALL( cvMoments( image->getImage(), moments, m_binary)); 
         if(cvGetSpatialMoment( moments, 0, 0 )!=0){
            OPENCV_CALL( x = (float)(cvGetSpatialMoment( moments, 1, 0 )/cvGetSpatialMoment( moments, 0, 0 )));
            OPENCV_CALL( y = (float)(cvGetSpatialMoment( moments, 0, 1 )/cvGetSpatialMoment( moments, 0, 0 )));
         }
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to calculate the gravity center: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         
         (*(outputs[m_xID].buffer))[count] = ObjectRef(Float::alloc(x)); 
         (*(outputs[m_yID].buffer))[count] = ObjectRef(Float::alloc(y)); 
      }
      
      NO_ORDER_NODE_SPEEDUP(CvGravityCenter)
   };
}//namespace FD
