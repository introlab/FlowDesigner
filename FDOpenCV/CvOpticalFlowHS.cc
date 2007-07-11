#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include <math.h>

using namespace std;

namespace FD {
   
   class CvOpticalFlowHS;
   
   DECLARE_NODE(CvOpticalFlowHS)
   /*Node
   *
   * @name CvOpticalFlowHS
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
   * @parameter_name USE_PREVIOUS
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Uses previous (input) velocity field.
   *
   * @parameter_name LAMBDA
   * @parameter_type float
   * @parameter_value 1
   * @parameter_description Lagrangian multiplier 
   * 
   * @parameter_name MAX_ITER
   * @parameter_type int
   * @parameter_value 10
   * @parameter_description maximum number of iterations.
   *
   * @parameter_name EPSILON
   * @parameter_type float
   * @parameter_value 1
   * @parameter_description accuracy to achieve
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
   
   class CvOpticalFlowHS : public BufferedNode {
      
      //Input ID
      int m_prevID;
      int m_currID;
      //Output ID
      int m_velxID;
      int m_velyID;
      //Parameters
      int m_usePrevious;
      float m_lambda;
      int m_maxIter;
      float m_epsilon;
      
      public:
      CvOpticalFlowHS(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_prevID = addInput("PREV");
         m_currID = addInput("CURR");
         
         //add outputs
         m_velxID = addOutput("VEL_X");
         m_velyID = addOutput("VEL_Y");

         //Initialize parameters         
         m_usePrevious = dereference_cast<int>(parameters.get("USE_PREVIOUS"));
         m_lambda = dereference_cast<float>(parameters.get("LAMBDA"));
         m_maxIter = dereference_cast<int>(parameters.get("MAX_ITER"));
         m_epsilon = dereference_cast<float>(parameters.get("EPSILON"));
         
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
         OPENCV_CALL( velx = new CvImage( cvCreateImage( cvGetSize(prevPtr->getImage()), IPL_DEPTH_32F, 1)) );
         OPENCV_CALL( vely = new CvImage( cvCreateImage( cvGetSize(prevPtr->getImage()), IPL_DEPTH_32F, 1)) );
         OPENCV_CALL( cvCalcOpticalFlowHS( prev->getImage(), curr->getImage()
            , m_usePrevious 
            , velx->getImage(), vely->getImage()
            , m_lambda
            , cvTermCriteria( CV_TERMCRIT_ITER+CV_TERMCRIT_EPS, m_maxIter, m_epsilon ))); 
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to calculate the optical flow: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         
         (*(outputs[m_velxID].buffer))[count] = ObjectRef(velx); 
         (*(outputs[m_velyID].buffer))[count] = ObjectRef(vely); 
      }
      NO_ORDER_NODE_SPEEDUP(CvOpticalFlowHS);
   };
}//namespace FD
