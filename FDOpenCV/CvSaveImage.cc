#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvSaveImage;
   
   DECLARE_NODE(CvSaveImage)
   /*Node
   *
   * @name CvSaveImage
   * @category FDOpenCV:Image
   * @description Save an image in a file
   *
   * @input_name IMAGE
   * @input_description Tmage to save
   * @input_type CvImage
   *
   * @input_name FILENAME
   * @input_description The file name to save
   * @input_type string
   * 
   * @output_name OUTPUT
   * @output_description The saved image
   * @output_type	CvImage
   * 
   END*/
   
   
   class CvSaveImage : public BufferedNode {
      
      int m_filenameID;
      int m_imageID;
      int m_outputID;
      
      public:
      CvSaveImage(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_filenameID = addInput("FILENAME");
         m_imageID = addInput("IMAGE");
         //add outputs
         m_outputID = addOutput("OUTPUT");
      }
      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<String> filename = getInput(m_filenameID,count);    
         RCPtr<CvImage> image = getInput(m_imageID,count);
         
         //Handle
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL(cvSaveImage( filename->c_str(), image->getImage()));     
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to save the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         
         out[count] = image;
         
      }
      
      NO_ORDER_NODE_SPEEDUP(CvSaveImage)
   };
}//namespace FD
