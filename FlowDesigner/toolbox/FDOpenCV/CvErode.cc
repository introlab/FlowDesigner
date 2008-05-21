#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include "CvStructuringElement.h"

using namespace std;

namespace FD {
   
   class CvErode;
   
   DECLARE_NODE(CvErode)
   /*Node
   *
   * @name CvErode
   * @category FDOpenCV:Morphological Operations
   * @description Erodes image by using arbitrary structuring element
   *
   * @input_name IMAGEIN
   * @input_description Input image
   * @input_type CvImage
   *
   * @input_name ELEMENT
   * @input_description Input element
   * @input_type CvStructuringElement
   *
   * @output_name IMAGEOUT
   * @output_description Output image
   * @output_type CvImage
   *
   * @parameter_name ITERATIONS
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Number of times dilation is applied
   *
   END*/
   
   
   class CvErode : public BufferedNode {
      //Input ID
      int m_imageInID;
      int m_elementID;
      //Output ID
      int m_imageOutID;
      //Parameters
      int m_iterations;
      
      public:
      CvErode(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageInID = addInput("IMAGEIN");
         m_elementID = addInput("ELEMENT");
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         //Initialize parameters         
         m_iterations = dereference_cast<int>(parameters.get("ITERATIONS"));
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);   
         RCPtr<CvStructuringElement> elementPtr = getInput(m_elementID,count);
         
         //Handle
         CvImage* image= new CvImage(&(*imagePtr));  
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__;
         OPENCV_CALL(cvErode( imagePtr->getImage(), image->getImage(), elementPtr->getStructuringElement(), m_iterations));
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to erode the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
         
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvErode)
   };
}//namespace FD
