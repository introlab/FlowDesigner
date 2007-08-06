#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvAvg;
   
   DECLARE_NODE(CvAvg)
   /*Node
   *
   * @name CvAvg
   * @category FDOpenCV:Arithmetic Logic Comparison
   * @description Calculates average (mean) of image.
   *
   * @input_name IMAGE
   * @input_description Input image.
   * @input_type CvImage
   *
   * @output_name AVG_1
   * @output_description Average (mean) of the channel 1 of the image.
   * @output_type float
   * 
   * @output_name AVG_2
   * @output_description Average (mean) of the channel 2 of the image.
   * @output_type float
   * 
   * @output_name AVG_3
   * @output_description Average (mean) of the channel 3 of the image.
   * @output_type float
   *
   END*/
   
   
   class CvAvg : public BufferedNode 
   {
      //Input ID            
      int m_imageID;
      //Output ID
      int m_avg1ID;
      int m_avg2ID;
      int m_avg3ID;
      
      public:
      CvAvg(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageID = addInput("IMAGE");
         //add outputs
         m_avg1ID = addOutput("AVG_1");
         m_avg2ID = addOutput("AVG_2");
         m_avg3ID = addOutput("AVG_3");
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageID,count);   
         //Handle
         
         CvScalar average = cvScalar(0,0,0,0);
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__;
         OPENCV_CALL(average = cvAvg( imagePtr->getImage() ));       
         __END__;
         cvSetErrMode( status );
         
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to mean the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
         
         (*(outputs[m_avg1ID].buffer))[count] = ObjectRef(Float::alloc( average.val[0]));
         (*(outputs[m_avg2ID].buffer))[count] = ObjectRef(Float::alloc( average.val[1]));
         (*(outputs[m_avg3ID].buffer))[count] = ObjectRef(Float::alloc( average.val[2]));
      }
      
      NO_ORDER_NODE_SPEEDUP(CvAvg)
   };
}//namespace FD
