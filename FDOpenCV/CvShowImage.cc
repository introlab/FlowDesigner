#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include <gdk/gdk.h>

using namespace std;

namespace FD {
   
   class CvShowImage;
   
   DECLARE_NODE(CvShowImage)
   /*Node
   *
   * @name CvShowImage
   * @category FDOpenCV:Image
   * @description View an image
   *
   * @input_name IMAGEIN
   * @input_description Image to visualize
   * @input_type CvImage
   *
   * @parameter_name WINDOW_TITLE
   * @parameter_type string
   * @parameter_value BaseImage
   * @parameter_description Window title.
   * 
   * @output_name IMAGEOUT
   * @output_description Same as input image 
   * @output_type CvImage
   *
   END*/
   
   
   class CvShowImage : public BufferedNode {
      
      int m_imageInID;
      int m_imageOutID;
      std::string m_windowName;
      
      
      public:
      CvShowImage(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageInID = addInput("IMAGEIN");
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");         
         //Initialize parameters         
         RCPtr<String> windowNamePtr = parameters.get("WINDOW_TITLE");
         //Create the windows
         m_windowName = *windowNamePtr;
         gdk_threads_enter();
         cvNamedWindow(m_windowName.c_str(), 1 );
         gdk_threads_leave();
      }
      
      ~CvShowImage()
      {
         gdk_threads_enter();
         cvDestroyWindow( m_windowName.c_str() );
         gdk_threads_leave();  
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);
         
         //Handle
         gdk_threads_enter();
         
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL(cvShowImage(m_windowName.c_str(), imagePtr->getImage()));     
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to show the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         
         gdk_threads_leave();
         out[count] = imagePtr;
      }
      
      NO_ORDER_NODE_SPEEDUP(CvShowImage)
   };
}//namespace FD
