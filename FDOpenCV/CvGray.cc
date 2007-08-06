#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvGray;
   
   DECLARE_NODE(CvGray)
   /*Node
   *
   * @name CvGray
   * @category FDOpenCV:Color
   * @description Implements Converts image from BGR to Gray
   *
   * @input_name IMAGEIN
   * @input_description Input image
   * @input_type CvImage
   * 
   * @output_name IMAGEOUT
   * @output_description The result gray image
   * @output_type CvImage
   *
   END*/
   
   
   class CvGray : public BufferedNode {
      //Input ID
      int m_imageInID;
      //Output ID
      int m_imageOutID;
      
      public:
      CvGray(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageInID = addInput("IMAGEIN");
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
      }      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);  
         //Handle
         out[count] = ObjectRef(imagePtr->gray());
      }
      
      NO_ORDER_NODE_SPEEDUP(CvGray)
   };
}//namespace FD
