#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include "CvStructuringElement.h"

using namespace std;

namespace FD {
   
   class CvDilate;
   
   DECLARE_NODE(CvDilate)
   /*Node
   *
   * @name CvDilate
   * @category FDOpenCV:Morphological Operations
   * @description Dilates image by using arbitrary structuring element
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
   
   
   class CvDilate : public BufferedNode {
      //Input ID
      int m_imageInID;
      int m_elementID;
      //Output ID
      int m_imageOutID;
      //Parameters
      int m_iterations;
      
      public:
      CvDilate(string nodeName, ParameterSet params)
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
         RCPtr<CvStructuringElement> elememtPtr = getInput(m_elementID,count);
         
         //Handle
         CvImage* image= new CvImage(imagePtr->getImage());  
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__;
         OPENCV_CALL(cvDilate(imagePtr->getImage(), image->getImage(), elememtPtr->getStructuringElement(), m_iterations));
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to dilate the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
         
         out[count] = image;
      }
      
      NO_ORDER_NODE_SPEEDUP(CvDilate)
   };
}//namespace FD
