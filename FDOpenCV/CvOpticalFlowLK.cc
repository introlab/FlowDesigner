#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include <math.h>

using namespace std;

namespace FD {
   
   class CvOpticalFlowLK;
   
   DECLARE_NODE(CvOpticalFlowLK)
   /*Node
   *
   * @name CvOpticalFlowLK
   * @category FDOpenCV:Video
   * @description Calculates optical flow for two images
   *
   * @input_name PREV
   * @input_description First image
   * @input_type CvImage
   *
   * @input_name CURR
   * @input_description Second image
   * @input_type CvImage
   *
   * @parameter_name WIN_SIZE_X
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description Size of the averaging window used for grouping pixels (width)
   *
   * @parameter_name WIN_SIZE_Y
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description Size of the averaging window used for grouping pixels (height)
   * 
   * @output_name VEL_X
   * @output_description Horizontal component of the optical flow
   * @output_type	CvImage
   * 
   * @output_name VEL_Y
   * @output_description Vertical component of the optical flow
   * @output_type	CvImage
   *
   END*/
   
   class CvOpticalFlowLK : public BufferedNode {
      
      //Input ID
      int m_prevID;
      int m_currID;
      //Output ID
      int m_velxID;
      int m_velyID;
      //Parameters
      int m_winSizeX;
      int m_winSizeY;
      
      public:
      CvOpticalFlowLK(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_prevID = addInput("PREV");
         m_currID = addInput("CURR");
         
         //add outputs
         m_velxID = addOutput("VEL_X");
         m_velyID = addOutput("VEL_Y");

         //Initialize parameters         
         m_winSizeX = dereference_cast<int>(parameters.get("WIN_SIZE_X"));
         m_winSizeY = dereference_cast<int>(parameters.get("WIN_SIZE_Y"));
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvImage> prevPtr = getInput(m_prevID,count);
         RCPtr<CvImage> currPtr = getInput(m_currID,count);
         
         //Handle
         
         CvImage* prev = prevPtr->gray();
         CvImage* curr = currPtr->gray();
         CvImage* velx;
         CvImage* vely;
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL( velx = new CvImage( cvGetSize(prevPtr->getImage()), IPL_DEPTH_32F, 1) );
         OPENCV_CALL( vely = new CvImage( cvGetSize(prevPtr->getImage()), IPL_DEPTH_32F, 1) );
         OPENCV_CALL( cvCalcOpticalFlowLK( prev->getImage(), curr->getImage()
            , cvSize(m_winSizeX,m_winSizeY)
            , velx->getImage(), vely->getImage() )); 
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to calculate the optical flow: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         delete prev;
         delete curr;
         (*(outputs[m_velxID].buffer))[count] = ObjectRef(velx); 
         (*(outputs[m_velyID].buffer))[count] = ObjectRef(vely); 
      }
      NO_ORDER_NODE_SPEEDUP(CvOpticalFlowLK);
   };
}//namespace FD
