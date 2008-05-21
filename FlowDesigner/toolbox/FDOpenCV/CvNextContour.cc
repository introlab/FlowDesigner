#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"

using namespace std;

namespace FD {
   
   class CvNextContour;
   
   DECLARE_NODE(CvNextContour)
   /*Node
   *
   * @name CvNextContour
   * @category FDOpenCV:Contours
   * @description Point to the next contour
   *
   * @input_name CONTOURSIN
   * @input_description The contour to handle
   * @input_type CvContours
   *
   * @input_name BOOL
   * @input_description If true, no change
   * @input_type bool
   *
   * @output_name CONTOURSOUT
   * @output_description The result contour
   * @output_type CvContours
   *
   * @output_name BOOL
   * @output_description true if the next contour exist. If false, the first contour is return
   * @output_type bool   
   *
   * @output_name NBELEMENTS
   * @output_description Number of elements in the selected contour
   * @output_type int
   * 
   END*/
   
   class CvNextContour : public BufferedNode {
      
      //Input ID
      int m_contoursInID;
      int m_boolInID;
      
      //Output ID
      int m_contoursOutID;
      int m_nbElementsID;
      int m_boolOutID;
      
      public:
      CvNextContour(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_contoursInID = addInput("CONTOURSIN");
         m_boolInID = addInput("BOOL");
         //add outputs
         m_contoursOutID = addOutput("CONTOURSOUT");
         m_nbElementsID = addOutput("NBELEMENTS");
         m_boolOutID = addOutput("BOOL");
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvContours> contoursPtr = getInput(m_contoursInID,count);
         RCPtr<Bool> boolPtr = getInput(m_boolInID,count);
         //Handle of the inputs
         bool Bool = true;
         int total;
         CvContours* contours = new CvContours(&(*contoursPtr)); 
         if((contoursPtr->getContours()) != 0)
         {          
            if( !(*boolPtr))
            {
               contours->setContours(contours->getContours()->h_next);
               if((contours->getContours()) == 0)
               {
                  Bool = false;
                  total = 0;
                  contours->setContours(contours->getFirstContours());
               }
               else
               {
                  total = contours->getContours()->total;               
               }
            }
         }
         else
         {
            Bool = false;
            total = 0;
         }
         
         (*(outputs[m_contoursOutID].buffer))[count] = ObjectRef(contours); 
         (*(outputs[m_boolOutID].buffer))[count] = ObjectRef(Bool::alloc(Bool));
         (*(outputs[m_nbElementsID].buffer))[count] = ObjectRef(Int::alloc( total ));
         
      }
      
      NO_ORDER_NODE_SPEEDUP(CvNextContour)
   };
}//namespace FD
