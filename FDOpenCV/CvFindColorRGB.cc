#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include "CvColor.h"

using namespace std;

namespace FD {
   
   class CvFindColorRGB;
   
   DECLARE_NODE(CvFindColorRGB)
   /*Node
   *
   * @name CvFindColorRGB
   * @category FDOpenCV
   * @description Detect the RGB color int the image
   *
   * @input_name IMAGEIN
   * @input_description The inptu Image
   * @input_type CvImage
   *
   * @input_name FIND_COLOR
   * @input_description Color to find
   * @input_type CvColor
   *
   * @parameter_name R_SENSIBILITY
   * @parameter_type int
   * @parameter_value 10
   * @parameter_description Sensibility with the red color
   *
   * @parameter_name G_SENSIBILITY
   * @parameter_type int
   * @parameter_value 10
   * @parameter_description Sensibility with the green color
   *
   * @parameter_name B_SENSIBILITY
   * @parameter_type int
   * @parameter_value 10
   * @parameter_description Sensibility with the blue color  
   * 
   * @output_name IMAGEOUT
   * @output_description The result image
   * @output_type	CvImage
   * 
   END*/
   inline std::string CINT(int value)
   {
      std::stringstream sstr;
      sstr << value;
      return(sstr.str());
   }   
   
   class CvFindColorRGB : public BufferedNode {
      
      //Input ID
      int m_imageInID;
      int m_findColorID;
      //Output ID
      int m_imageOutID;
      //Parameter
      int m_RSensibility;
      int m_GSensibility;
      int m_BSensibility;
      
      public:
      CvFindColorRGB(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageInID = addInput("IMAGEIN");
         m_findColorID = addInput("FIND_COLOR");
         
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         
         //Initialize parameters         
         m_RSensibility = dereference_cast<int>(parameters.get("R_SENSIBILITY"));
         m_GSensibility = dereference_cast<int>(parameters.get("G_SENSIBILITY"));
         m_BSensibility = dereference_cast<int>(parameters.get("B_SENSIBILITY"));         
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs         
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);
         RCPtr<CvColor> findColorPtr = getInput(m_findColorID,count);         
         IplImage* temp; 
         cvSetErrMode( CV_ErrModeSilent );
         OPENCV_CALL(temp = cvCreateImage( cvGetSize((imagePtr->getImage())), IPL_DEPTH_8U, 1));
         cvSetErrMode( CV_ErrModeLeaf );         
         if( cvGetErrStatus() == CV_StsOk  )
         {
            cvZero(temp);
            unsigned char B,G,R;
            unsigned int x,y;
            int d=20;            
            for (x=0; x<(imagePtr->getImage())->width ; x++)
            {
               for (y=0; y<(imagePtr->getImage())->height ; y++)
               {
                  B = ((uchar*)((imagePtr->getImage())->imageData + (imagePtr->getImage())->widthStep*y))[x*3];
                  G = ((uchar*)((imagePtr->getImage())->imageData + (imagePtr->getImage())->widthStep*y))[x*3+1];
                  R = ((uchar*)((imagePtr->getImage())->imageData + (imagePtr->getImage())->widthStep*y))[x*3+2];
                  
                  if( ((findColorPtr->channel3()-m_BSensibility)<=B & B<=(findColorPtr->channel3()+m_BSensibility)) & 
                     ((findColorPtr->channel2()-m_GSensibility)<=G & G<=(findColorPtr->channel2()+m_GSensibility)) &
                  ((findColorPtr->channel1()-m_RSensibility)<=R & R<=(findColorPtr->channel1()+m_RSensibility)) )
                  {
                     ((uchar*)(temp->imageData + temp->widthStep*y))[x]=255;
                  }
               }
            } 
         }
         else
         {
            throw new GeneralException("OPENCV - Error to find colors: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }         
         out[count] = ObjectRef(new CvImage(temp));
      }
      
      NO_ORDER_NODE_SPEEDUP(CvFindColorRGB)
   };
}//namespace FD