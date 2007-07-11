#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include <map>
#include <string>

using namespace std;

namespace FD {
   
   class CvThreshold;
   
   DECLARE_NODE(CvThreshold)
   /*Node
   *
   * @name CvThreshold
   * @category FDOpenCV:Morphological Operations
   * @description Applies fixed-level threshold to array elements
   *
   * @input_name IMAGEIN
   * @input_description Input image
   * @input_type CvImage
   * 
   * @output_name IMAGEOUT
   * @output_description Output image
   * @output_type CvImage
   *
   * @parameter_name THRESHOLD
   * @parameter_type float
   * @parameter_value 50
   * @parameter_description Threshold value
   *
   * @parameter_name MAX_VALUE
   * @parameter_type float
   * @parameter_value 255
   * @parameter_description Maximum value to use with CV_THRESH_BINARY and CV_THRESH_BINARY_INV thresholding types
   *
   * @parameter_name THRESHOLD_TYPE
   * @parameter_type string
   * @parameter_value CV_THRESH_BINARY;CV_THRESH_BINARY_INV;CV_THRESH_TRUNC;CV_THRESH_TOZERO;CV_THRESH_TOZERO_INV
   * @parameter_description Thresholding type
   END*/
   
   
   class CvThreshold : public BufferedNode {
      //Input ID
      int m_imageInID;
      //Output ID
      int m_imageOutID;
      //Parameters
      float m_threshold;
      float m_max_value;
      string m_thresholdType;
      //Initialize the thresholeType map
      map<string,int> m_thresholeTypeMap;
      
      
      public:
      CvThreshold(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         
         m_imageInID = addInput("IMAGEIN");
         m_imageOutID = addOutput("IMAGEOUT");
         m_threshold = dereference_cast<float>(parameters.get("THRESHOLD"));
         m_max_value = dereference_cast<float>(parameters.get("MAX_VALUE"));
         m_thresholdType = object_cast<String>(parameters.get("THRESHOLD_TYPE"));
         
         //initialize type map
         m_thresholeTypeMap["CV_THRESH_BINARY"] = CV_THRESH_BINARY;
         m_thresholeTypeMap["CV_THRESH_BINARY_INV"] = CV_THRESH_BINARY_INV;
         m_thresholeTypeMap["CV_THRESH_TRUNC"] = CV_THRESH_TRUNC;
         m_thresholeTypeMap["CV_THRESH_TOZERO"] = CV_THRESH_TOZERO;
         m_thresholeTypeMap["CV_THRESH_TOZERO_INV"] = CV_THRESH_TOZERO_INV;
         
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);
         CvImage* imageGRAY = imagePtr->gray(); 
         CvImage* image =  new CvImage(cvCreateImage(cvSize(imagePtr->getImage()->width, imagePtr->getImage()->height)
            , IPL_DEPTH_8U, 1));
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL(cvThreshold(imageGRAY->getImage(), image->getImage(), m_threshold, m_max_value, m_thresholeTypeMap[m_thresholdType]));         
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to threshold the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvThreshold)
   };
}//namespace FD
