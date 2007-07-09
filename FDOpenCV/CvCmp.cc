#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvCmp;
   
   DECLARE_NODE(CvCmp)
   /*Node
   *
   * @name CvCmp
   * @category FDOpenCV:Arithmetic Logic Comparison
   * @description Performs per-element comparison of two images
   * dst(I) is set to 0xff (all '1'-bits) if the particular relation between the elements is true and 0 otherwise.
   * All the images must have the same type, except the destination, and the same size.
   *
   * @input_name IMAGEIN1
   * @input_description Input image1.
   * @input_type CvImage
   *
   * @input_name IMAGEIN2
   * @input_description Input image2.
   * @input_type CvImage
   *
   * @parameter_name CMP_OP
   * @parameter_type string
   * @parameter_value CV_CMP_EQ;CV_CMP_GT;CV_CMP_GE;CV_CMP_LT;CV_CMP_LE;CV_CMP_NE
   * @parameter_description : The flag specifying the relation between the elements to be checked
   *
   * @output_name IMAGEOUT
   * @output_description Output image
   * @output_type CvImage
   *
   *
   END*/
   
   
   class CvCmp : public BufferedNode 
   {
      //Input ID            
      int m_imageIn1ID;
      int m_imageIn2ID;
      //Output ID
      int m_imageOutID;
      //Parameters
      string m_cmpOp;
      //map of the line_type
      map<string,int> m_cmpOpMap;      
      
      public:
      CvCmp(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageIn1ID = addInput("IMAGEIN1");
         m_imageIn2ID = addInput("IMAGEIN2");
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         //Initialize parameters         
         m_cmpOp = object_cast<String>(parameters.get("CMP_OP"));         
         
         //Initialize the cmpOp map
         m_cmpOpMap["CV_CMP_EQ"] = CV_CMP_EQ;
         m_cmpOpMap["CV_CMP_GT"] = CV_CMP_GT;
         m_cmpOpMap["CV_CMP_GE"] = CV_CMP_GE;           
         m_cmpOpMap["CV_CMP_LT"] = CV_CMP_LT;
         m_cmpOpMap["CV_CMP_LE"] = CV_CMP_LE;
         m_cmpOpMap["CV_CMP_NE"] = CV_CMP_NE;          
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> imageIn1Ptr = getInput(m_imageIn1ID,count);   
         RCPtr<CvImage> imageIn2Ptr = getInput(m_imageIn2ID,count);
         //Handle
         CvImage* image = new CvImage(imageIn1Ptr->getImage());
         if(imageIn1Ptr->getImage()->nChannels == 1 && imageIn2Ptr->getImage()->nChannels == 1)
         {
            cvSetErrMode( CV_ErrModeSilent );
            __BEGIN__;
            OPENCV_CALL( cvCmp( imageIn1Ptr->getImage(), imageIn2Ptr->getImage(), image->getImage(), m_cmpOpMap[m_cmpOp] ));          
            __END__;
            cvSetErrMode( CV_ErrModeLeaf );
            if( cvGetErrStatus() != CV_StsOk  )
            {
               throw new GeneralException("OPENCV - Error to compare the images: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
            }
         }
         else
         {
            throw new GeneralException("OPENCV - Error to compare the images: they must be single-channel",__FILE__,__LINE__);
         }
         
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvCmp)
   };
}//namespace FD
