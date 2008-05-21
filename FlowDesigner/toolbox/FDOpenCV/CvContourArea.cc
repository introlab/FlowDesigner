#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"

using namespace std;

namespace FD {
   
   class CvContourArea;
   
   DECLARE_NODE(CvContourArea)
   /*Node
   *
   * @name CvContourArea
   * @category FDOpenCV:Contours
   * @description Calculate the contour area
   *
   * @input_name CONTOURS
   * @input_description The contours to handle
   * @input_type CvContours
   *
   * @output_name AREA
   * @output_description the contour area
   * @output_type float
   *
   END*/
   
   class CvContourArea : public BufferedNode {
      
      //Input ID
      int m_contoursID;
      //Output ID
      int m_areaID;
      
      public:
      CvContourArea(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_contoursID = addInput("CONTOURS");
         
         //add outputs
         m_areaID = addOutput("AREA");       
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvContours> contoursPtr = getInput(m_contoursID,count);
         
         //Handle
         float area;
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent ); 
         __BEGIN__;
         OPENCV_CALL(area = (float)(fabs(cvContourArea( contoursPtr->getContours()))));
         __END__;
         cvSetErrMode( status );
         
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to calculate the area of the contour: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
         
         out[count] = ObjectRef(Float::alloc(area));
      }
      
      NO_ORDER_NODE_SPEEDUP(CvContourArea)
   };
}//namespace FD
