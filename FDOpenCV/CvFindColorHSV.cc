#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include "CvColor.h"

using namespace std;

namespace FD {
   
   class CvFindColorHSV;
   
   DECLARE_NODE(CvFindColorHSV)
   /*Node
   *
   * @name CvFindColorHSV
   * @category FDOpenCV:Color
   * @description Detects the color in the image with a HSV method
   * dst(I) is set to 0xff (all '1'-bits) if the color is detect and 0 otherwise.
   * 
   * @input_name IMAGEIN
   * @input_description Input image
   * @input_type CvImage
   *
   * @input_name COLORMIN
   * @input_description Color which represents the minimal value for each channel.
   * @input_type CvColor
   *
   * @input_name COLORMAX
   * @input_description Color which represents the maximal value for each channel.
   * @input_type CvColor   
   * 
   * @output_name IMAGEOUT
   * @output_description Result image
   * @output_type	CvImage
   * 
   END*/
   inline std::string CINT(int value)
   {
      std::stringstream sstr;
      sstr << value;
      return(sstr.str());
   }   
   
   class CvFindColorHSV : public BufferedNode {
      
      //Input ID
      int m_imageInID;
      int m_colorMinID;
      int m_colorMaxID;
      //Output ID
      int m_imageOutID;
      
      public:
      CvFindColorHSV(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageInID = addInput("IMAGEIN");
         m_colorMinID = addInput("COLORMIN");
         m_colorMaxID = addInput("COLORMAX");
         
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs         
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);
         RCPtr<CvColor> colorMinPtr = getInput(m_colorMinID,count);
         RCPtr<CvColor> colorMaxPtr = getInput(m_colorMaxID,count);
         
         IplImage* temp;
         IplImage* imageHSV; 
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__;          
         
         OPENCV_CALL( temp = cvCreateImage( cvGetSize(imagePtr->getImage()), IPL_DEPTH_8U, 1) ); 
         OPENCV_CALL(cvZero(temp));
         OPENCV_CALL( imageHSV = cvCreateImage( cvGetSize(imagePtr->getImage()), IPL_DEPTH_8U, 3));
         OPENCV_CALL( cvCvtColor( imagePtr->getImage(), imageHSV, CV_BGR2HSV )); 
         
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to find the color: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
          
         unsigned char H,S,V;
         bool h;
         int x,y;
         for (x=0; x<=imageHSV->width ; x++)
         {
            for (y=0; y<=imageHSV->height ; y++)
            {
               H = ((uchar*)(imageHSV->imageData + imageHSV->widthStep*y))[x*3];
               S = ((uchar*)(imageHSV->imageData + imageHSV->widthStep*y))[x*3+1];
               V = ((uchar*)(imageHSV->imageData + imageHSV->widthStep*y))[x*3+2];

               if ( colorMaxPtr->channel1() > colorMinPtr->channel1())
               {
                  h = ( H*2 <= colorMaxPtr->channel1() ) && ( H*2 >= colorMinPtr->channel1() );
               }
               else
               {
                  h = ( H*2 <= colorMaxPtr->channel1() ) || ( H*2 >= colorMinPtr->channel1() );
               }
               
               if( h
                  && ( S <= colorMaxPtr->channel2()) && ( S >= colorMinPtr->channel2() )
               && ( V <= colorMaxPtr->channel3() ) && ( V >= colorMinPtr->channel3() ) )
               
               {
                  ((uchar*)(temp->imageData + temp->widthStep*y))[x]=255;
               }
               else    
               {
               }
            }
         } 
         
         out[count] = ObjectRef(new CvImage(temp));
      }
      
      NO_ORDER_NODE_SPEEDUP(CvFindColorHSV)
   };
}//namespace FD
