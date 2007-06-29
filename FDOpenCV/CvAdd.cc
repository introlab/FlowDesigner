#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvAdd;
   
   DECLARE_NODE(CvAdd)
   /*Node
   *
   * @name CvAdd
   * @category FDOpenCV:Arithmetic_Logic_Comparison
   * @description Implements Canny algorithm for edge detection
   *
   * @input_name IMAGE1
   * @input_description Input image1
   * @input_type CvImage
   *
   * @input_name IMAGE2
   * @input_description Input image2
   * @input_type CvImage   
   * 
   * @output_name OUTPUT
   * @output_description Output image
   * @output_type CvImage
   *
   *
   END*/
   
   
   class CvAdd : public BufferedNode 
   {
      //Input ID            
      int m_image1ID;
      int m_image2ID;
      //Output ID
      int m_outputID;
      
      public:
      CvAdd(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_image1ID = addInput("IMAGE1");
         m_image2ID = addInput("IMAGE2");
         //add outputs
         m_outputID = addOutput("OUTPUT");
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> image1Ptr = getInput(m_image1ID,count);   
         RCPtr<CvImage> image2Ptr = getInput(m_image2ID,count);
         //Handle
         CvImage* image;
         
         cvSetErrMode( CV_ErrModeSilent );
         image = new CvImage(*image1Ptr);
         __BEGIN__;
         OPENCV_CALL(cvAdd(image1Ptr->getImage(), image2Ptr->getImage(), image->getImage()));          
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to add the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
         
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvAdd)
   };
}//namespace FD
