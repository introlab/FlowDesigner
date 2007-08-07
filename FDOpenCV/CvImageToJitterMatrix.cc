#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include <JitterMatrix.h>
using namespace std;

namespace FD {
   
   class CvImageToJitterMatrix;
   
   DECLARE_NODE(CvImageToJitterMatrix)
   /*Node
   *
   * @name CvImageToJitterMatrix
   * @category FDOpenCV:MAX
   * @description Convert an image to a jitterMatrix
   *
   * @input_name IMAGE
   * @input_description Input image
   * @input_type CvImage
   *
   * @output_name JITTERMATRIX
   * @output_description The result jitterMatrix
   * @output_type	JitterMatrix
   *
   END*/
   
   class CvImageToJitterMatrix : public BufferedNode {
      
      //Input ID
      int m_imageID;
      //Output ID
      int m_jitterMatrixID;
     
      public:
      CvImageToJitterMatrix(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         
         //add Outputs
         m_jitterMatrixID = addOutput("JITTERMATRIX");
         
         //add Intputs
         m_imageID = addInput("IMAGE");
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {       
         RCPtr<CvImage> imagePtr = getInput(m_imageID,count);
         //Handle of the inputs
         IplImage* image=0;
         if(imagePtr->getImage()== NULL)
         {
            out[count] = ObjectRef(new JitterMatrix());
            return;                       
         }
         if(imagePtr->getImage()->nChannels == 3) 
         {
            image = cvCreateImage( cvSize(imagePtr->getImage()->width, imagePtr->getImage()->height)
                                    , imagePtr->getImage()->depth
                                    , 4);
            IplImage* alpha = cvCreateImage( cvSize(imagePtr->getImage()->width, imagePtr->getImage()->height)
                                    , imagePtr->getImage()->depth, 1);
            cvSet(alpha,cvScalar(255));
            IplImage* in[] = { imagePtr->getImage(), alpha }; 
            IplImage* out[] = { image }; 
            int from_to[] = { 0, 3, 1, 2, 2, 1, 3, 0  };
            cvMixChannels( (const CvArr**)in, 2, (CvArr**)out, 1, from_to, 4 );  
            cvReleaseImage(&alpha);          
         }               
         
         long type=setType(image->depth);
         
         JitterMatrix matrix( image->width, image->height, image->nChannels, 1444, type, image->imageData );
         if(image!=0)
         {
            cvReleaseImage(&image);
         }
         
         out[count] = ObjectRef(new JitterMatrix(matrix));
      }
      
      long setType(int type)
      {
         if( type==IPL_DEPTH_16U || type==IPL_DEPTH_16S || type==IPL_DEPTH_32S )
         {
            return 1; // 1 -> long            
         }
         else if( type==type==IPL_DEPTH_32F )
         {
            return 2; // 2 -> float32
         }
         else if( type==IPL_DEPTH_64F )
         {
            return 3; // 3 -> float64
         }
         return 0; // 0 -> char
      }
      NO_ORDER_NODE_SPEEDUP(CvImageToJitterMatrix)
   };
}//namespace FD
