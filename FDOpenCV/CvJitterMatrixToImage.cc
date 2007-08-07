#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include <JitterMatrix.h>
using namespace std;

namespace FD {
   
   class CvJitterMatrixToImage;
   
   DECLARE_NODE(CvJitterMatrixToImage)
   /*Node
   *
   * @name CvJitterMatrixToImage
   * @category FDOpenCV:MAX
   * @description Creates an image : header and allocates data
   *
   * @input_name JITTERMATRIX
   * @input_description Input image
   * @input_type JitterMatrix
   *
   * @output_name IMAGE
   * @output_description The result image
   * @output_type CvImage
   * 
   END*/
   
   class CvJitterMatrixToImage : public BufferedNode {
      
      //Input ID
      int m_jitterMatrixID;
      //Output ID
      int m_imageID;
      
      public:
      CvJitterMatrixToImage(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         
         //add Inputs
         m_jitterMatrixID = addInput("JITTERMATRIX");
         
         //add outputs
         m_imageID = addOutput("IMAGE");
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {     
         
         //Handle of the inputs
         RCPtr<JitterMatrix> matrixPtr = getInput(m_jitterMatrixID,count);
         
         IplImage* imageIn=NULL;
         IplImage* imageOut=NULL;
         if((matrixPtr->getMatrix()).ID == "JMTX")
         {
            if(matrixPtr->getPlaneCount() == 4) 
            {
               imageOut = cvCreateImage( cvSize( matrixPtr->getWidth(), matrixPtr->getHeight()), IPL_DEPTH_8U, 3);
               imageIn  = cvCreateImage( cvSize( matrixPtr->getWidth(), matrixPtr->getHeight()), IPL_DEPTH_8U, 4);
               imageIn->imageData = matrixPtr->getBuffer();
               int from_to[] = { 1, 2, 2, 1, 3, 0  };
               cvMixChannels( (const CvArr**)&imageIn, 1, (CvArr**)&imageOut, 1, from_to, 3 );            
            }  
         }
         else
         {
            imageOut = NULL;   
         }
         
         out[count] = ObjectRef(new CvImage(imageOut));
         if(imageIn != NULL)
         {
            cvReleaseImage( &imageIn );
         }
         if(imageOut != NULL)
         {
            cvReleaseImage( &imageOut );
         }
      }     
      
      NO_ORDER_NODE_SPEEDUP(CvJitterMatrixToImage)
   };
}//namespace FD
