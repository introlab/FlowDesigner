#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvSmooth;
   
   DECLARE_NODE(CvSmooth)
   /*Node
   *
   * @name CvSmooth
   * @category FDOpenCV:Morphological Operations
   * @description Smooths the image
   *
   * @input_name IMAGEIN
   * @input_description Input image
   * @input_type CvImage
   * 
   * @output_name IMAGEOUT
   * @output_description Output image
   * @output_type CvImage
   *
   * @parameter_name SMOOTH_TYPE
   * @parameter_type string
   * @parameter_value CV_BLUR_NO_SCALE;CV_BLUR;CV_GAUSSIAN;CV_MEDIAN;CV_BILATERAL
   * @parameter_description Type of the smoothing
   * CV_BLUR_NO_SCALE (simple blur with no scaling) - summation over a pixel param1×param2 neighborhood. If the neighborhood size may vary, one may precompute integral image with cvIntegral function.
   * CV_BLUR (simple blur) - summation over a pixel param1xparam2 neighborhood with subsequent scaling by 1/(param1*param2).
   * CV_GAUSSIAN (gaussian blur) - convolving image with param1×param2 Gaussian kernel.
   * CV_MEDIAN (median blur) - finding median of param1×param1 neighborhood (i.e. the neighborhood is square).
   * CV_BILATERAL (bilateral filter) - applying bilateral 3x3 filtering with color sigma=param1 and space sigma=param2.    
   *
   * @parameter_name PARAM1
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description The first parameter of smoothing operation
   *
   * @parameter_name PARAM2
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description The second parameter of smoothing operation. In case of simple scaled/non-scaled and Gaussian blur if param2 is zero, it is set to param1
   *
   * @parameter_name PARAM3
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description In case of Gaussian parameter this parameter may specify Gaussian sigma (standard deviation). If it is zero, it is calculated from the kernel size
   END*/
   
   
   class CvSmooth : public BufferedNode {
      //Input ID
      int m_imageInID;
      //Output ID
      int m_imageOutID;
      //Parameters
      string m_smoothType;
      int m_param1;
      int m_param2;
      int m_param3;
      //map of the smooth_type
      map<string,int> m_smoothTypeMap;
      
      public:
      CvSmooth(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageInID = addInput("IMAGEIN");
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         //Initialize parameters
         m_param1 = dereference_cast<int>(parameters.get("PARAM1"));
         m_param2 = dereference_cast<int>(parameters.get("PARAM2"));
         m_param3 = dereference_cast<int>(parameters.get("PARAM3"));
         m_smoothType = object_cast<String>(parameters.get("SMOOTH_TYPE"));         
         
         //Initialize the smoothtype map
         m_smoothTypeMap["CV_BLUR_NO_SCALE"] = CV_BLUR_NO_SCALE;
         m_smoothTypeMap["CV_BLUR"] = CV_BLUR;
         m_smoothTypeMap["CV_GAUSSIAN"] = CV_GAUSSIAN; 
         m_smoothTypeMap["CV_MEDIAN"] = CV_MEDIAN;
         m_smoothTypeMap["CV_BILATERAL"] = CV_BILATERAL;         
         
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);  
         
         //Handle
         CvImage* image = new CvImage(imagePtr->getImage());
         
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL( cvSmooth( imagePtr->getImage(), image->getImage(),
            m_smoothTypeMap[m_smoothType],
            m_param1, m_param2, m_param3 ));         
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to smooth the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }         
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvSmooth)
   };
}//namespace FD
