#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvSobel;
   
   DECLARE_NODE(CvSobel)
   /*Node
   *
   * @name CvSobel
   * @category FDOpenCV:Morphological_Operations
   * @description Calculates first, second, third or mixed image derivatives using extended Sobel operator
   *
   * @input_name IMAGEIN
   * @input_description Input image
   * @input_type CvImage
   * 
   * @output_name IMAGEOUT
   * @output_description Image of the edges found by the function.
   * @output_type CvImage
   *
   * @parameter_name XORDER
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description The first threshold
   *
   * @parameter_name YORDER
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description The second threshold   
   *
   * @parameter_name APERTURE_SIZE
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description Aperture parameter for Sobel operator
   END*/
   
   
   class CvSobel : public BufferedNode {
      //Input ID
      int m_imageInID;
      //Output ID
      int m_imageOutID;
      //Parameters
      int m_xorder;
      int m_yorder;
      int m_aperture_size;
      
      public:
      CvSobel(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageInID = addInput("IMAGEIN");
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         //Initialize parameters
         m_xorder = dereference_cast<int>(parameters.get("XORDER"));
         m_yorder = dereference_cast<int>(parameters.get("YORDER"));
         m_aperture_size = dereference_cast<int>(parameters.get("APERTURE_SIZE"));
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count); 
         
         //Handle
         CvImage* image = imagePtr->gray();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         if((image->getImage())->depth < 16)
         {
            CvImage* imageTemp;
            OPENCV_CALL(imageTemp = new CvImage(cvCreateImage( cvGetSize(image->getImage()), IPL_DEPTH_16S, 1)));
            OPENCV_CALL(cvSobel(image->getImage(), imageTemp->getImage(), m_xorder, m_yorder, m_aperture_size));
            OPENCV_CALL(cvConvertScaleAbs( imageTemp->getImage(),image->getImage())); 
         }
         else
         {
            OPENCV_CALL(cvSobel(image->getImage(), image->getImage(), m_xorder, m_yorder, m_aperture_size));
         }   
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to sobel the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvSobel)
   };
}//namespace FD
