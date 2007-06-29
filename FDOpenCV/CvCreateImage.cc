#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvCreateImage;
   
   DECLARE_NODE(CvCreateImage)
   /*Node
   *
   * @name CvCreateImage
   * @category FDOpenCV:Image
   * @description Creates an image : header and allocates data
   *
   * @parameter_name WIDTH
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Width of the image
   *
   * @parameter_name HEIGHT
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Height of the image
   *
   * @parameter_name DEPTH
   * @parameter_type string
   * @parameter_value IPL_DEPTH_8U;IPL_DEPTH_8S;IPL_DEPTH_16U;IPL_DEPTH_16S;IPL_DEPTH_32S;IPL_DEPTH_32F;IPL_DEPTH_64F
   * @parameter_description Bit depth of image elements
   *
   * @parameter_name CHANNELS
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Number of channels per element(pixel). Can be 1, 2, 3 or 4
   *
   * @output_name IMAGE
   * @output_description The result image
   * @output_type	CvImage
   * 
   END*/
   
   class CvCreateImage : public BufferedNode {
      
      //Output ID
      int m_imageID;
      //Parameter
      int m_width;
      int m_height;
      string m_depth;
      int m_channels;
      //map of the depth
      map<string,int> m_depthMap;
      
      public:
      CvCreateImage(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         
         //add outputs
         m_imageID = addOutput("IMAGE");
         
         //Initialize parameters         
         m_width = dereference_cast<int>(parameters.get("WIDTH"));
         m_height = dereference_cast<int>(parameters.get("HEIGHT"));
         m_depth = object_cast<String>(parameters.get("DEPTH")); 
         m_channels = dereference_cast<int>(parameters.get("CHANNELS"));
         
         //Initialize lineType map
         m_depthMap["IPL_DEPTH_8U"] = IPL_DEPTH_8U;
         m_depthMap["IPL_DEPTH_8S"] = IPL_DEPTH_8S;
         m_depthMap["IPL_DEPTH_16U"] = IPL_DEPTH_16U;
         m_depthMap["IPL_DEPTH_16S"] = IPL_DEPTH_16S;
         m_depthMap["IPL_DEPTH_32S"] = IPL_DEPTH_32S;
         m_depthMap["IPL_DEPTH_32F"] = IPL_DEPTH_32F;
         m_depthMap["IPL_DEPTH_64F"] = IPL_DEPTH_64F;
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {       
         //Handle of the inputs
         IplImage* temp;

         cvSetErrMode( CV_ErrModeSilent ); 
         __BEGIN__;
         OPENCV_CALL(temp = cvCreateImage( cvSize(m_width,m_height) , m_depthMap[m_depth], m_channels ));
         OPENCV_CALL(cvZero(temp));
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to create the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }
         
         
         CvImage* image = new CvImage(temp);
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvCreateImage)
   };
}//namespace FD
