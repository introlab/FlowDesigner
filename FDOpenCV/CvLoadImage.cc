#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   using namespace std;
   
   class CvLoadImage;
   
   DECLARE_NODE(CvLoadImage)
   /*Node
   *
   * @name CvLoadImage
   * @category FDOpenCV:Image
   * @description Load an image from a file
   *
   * @input_name FILENAME
   * @input_description The file name to load
   * @input_type string
   *
   * @parameter_name ISCOLOR
   * @parameter_type int
   * @parameter_value -1
   * @parameter_description Specifies colorness of the loaded image   
   *
   * @output_name IMAGE
   * @output_description The loaded image data structure
   * @output_type	CvImage
   * 
   END*/
   
   
   class CvLoadImage : public BufferedNode {
      //Input ID
      int m_filenameID;
      //Output ID
      int m_imageID;
      //Parameters
      int m_iscolor;
      
      public:
      CvLoadImage(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_filenameID = addInput("FILENAME");
         //add outputs
         m_imageID = addOutput("IMAGE");
         //Initialize parameters         
         m_iscolor = dereference_cast<int>(parameters.get("ISCOLOR"));
      }
      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<String> filename = getInput(m_filenameID,count); 
         
         //Handle
         RCPtr<CvImage> imagePtr(new CvImage(*filename, m_iscolor));       
         out[count] = imagePtr;
      }
      
      NO_ORDER_NODE_SPEEDUP(CvLoadImage)
   };
}//namespace FD
