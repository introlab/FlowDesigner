#include "BufferedNode.h"
#include "operators.h"
#include "Exception.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvCanny;
   
   DECLARE_NODE(CvCanny)
   /*Node
   *
   * @name CvCanny
   * @category FDOpenCV:Morphological Operations
   * @description Implements Canny algorithm for edge detection
   * The function cvCanny finds the edges on the input image image and marks them in the output image edges using the Canny algorithm.
   * The smallest of threshold1 and threshold2 is used for edge linking, the largest - to find initial segments of strong edges.
   *
   * @input_name INPUT
   * @input_description Input image
   * @input_type CvImage
   * 
   * @output_name OUTPUT
   * @output_description Image of the edges found by the function.
   * @output_type CvImage
   *
   * @parameter_name THRESHOLD1
   * @parameter_type float
   * @parameter_value 50
   * @parameter_description The first threshold
   *
   * @parameter_name THRESHOLD2
   * @parameter_type float
   * @parameter_value 200
   * @parameter_description The second threshold   
   *
   * @parameter_name APERTURE_SIZE
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description Aperture parameter for Sobel operator
   END*/
   
   
   class CvCanny : public BufferedNode {
      //Input ID
      int m_inputID;
      //Output ID
      int m_outputID;
      //Parameters
      float m_threshold1;
      float m_threshold2;
      int m_aperture_size;
      
      public:
      CvCanny(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_inputID = addInput("INPUT");
         //add outputs
         m_outputID = addOutput("OUTPUT");
         //Initialize parameters         
         m_threshold1 = dereference_cast<float>(parameters.get("THRESHOLD1"));
         m_threshold2 = dereference_cast<float>(parameters.get("THRESHOLD2"));
         m_aperture_size = dereference_cast<int>(parameters.get("APERTURE_SIZE"));
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_inputID,count);  
         
         //Handle
         CvImage* imageGray = imagePtr->gray(); 
         CvImage* image = new CvImage(imageGray); 
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent ); 
         __BEGIN__;
         OPENCV_CALL(cvCanny(imageGray->getImage(), image->getImage(), m_threshold1, m_threshold2, m_aperture_size));
         __END__;
         cvSetErrMode( status );
         delete imageGray;         
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to apply Canny on the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
         
         out[count] = ObjectRef(image);         
      }
      
      NO_ORDER_NODE_SPEEDUP(CvCanny)
   };
}//namespace FD
