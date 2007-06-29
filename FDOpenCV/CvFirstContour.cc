#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"

using namespace std;

namespace FD {
   
   class CvFirstContour;
   
   DECLARE_NODE(CvFirstContour)
   /*Node
   *
   * @name CvFirstContour
   * @category FDOpenCV:Contours
   * @description Point to the first contour
   *
   * @input_name CONTOURSIN
   * @input_description The contour to handle
   * @input_type CvContours
   *
   * @output_name CONTOURSOUT
   * @output_description The result contour
   * @output_type	CvContours
   *
   END*/
   
   class CvFirstContour : public BufferedNode {
      
      //Input ID
      int m_contoursInID;

      //Output ID
      int m_contoursOutID;
      int m_nbElementsID;
      
      
      public:
      CvFirstContour(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_contoursInID = addInput("CONTOURSIN");
         
         //add outputs
         m_contoursOutID = addOutput("CONTOURSOUT");
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvContours> contoursPtr = getInput(m_contoursInID,count);
         //Handle of the inputs
                  if((contoursPtr->getContours()) == 0)
         {
            throw new GeneralException("OPENCV - Contour doesn't exist",__FILE__,__LINE__);
         } 
         CvContours* contours = new  CvContours(&(*contoursPtr));         
         contours->setContours(contours->getFirstContours());         
         out[count] = ObjectRef(contours); 

         
      }
      
      NO_ORDER_NODE_SPEEDUP(CvFirstContour)
   };
}//namespace FD
