#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"

using namespace std;

namespace FD {
   
   class CvNextHierarchy;
   
   DECLARE_NODE(CvNextHierarchy)
   /*Node
   *
   * @name CvNextHierarchy
   * @category FDOpenCV:Contours
   * @description Point to the next hierarchy
   *
   * @input_name CONTOURSIN
   * @input_description The contour to handle
   * @input_type CvContours
   * 
   * @output_name CONTOURSOUT
   * @output_description The result contour
   * @output_type CvContours
   *
   * @output_name BOOL
   * @output_description True if the next hierarchy exist. If false, the first contour is return
   * @output_type bool
   *
   * @output_name NBELEMENTS
   * @output_description Number of elements in the selected contour
   * @output_type int
   * 
   END*/
   
   class CvNextHierarchy : public BufferedNode {
      
      //Input ID
      int m_contoursInID;
      
      //Output ID
      int m_contoursOutID;
      int m_nbElementsID;
      int m_boolID;
      
      
      public:
      CvNextHierarchy(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_contoursInID = addInput("CONTOURSIN");
         
         //add outputs
         m_contoursOutID = addOutput("CONTOURSOUT");
         m_nbElementsID = addOutput("NBELEMENTS");
         m_boolID = addOutput("BOOL");
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvContours> contoursPtr = getInput(m_contoursInID,count);
         //Handle of the inputs
         bool Bool = true;
         int total;
         CvContours* contours = new CvContours(&(*contoursPtr));         
         if((contoursPtr->getContours()) != 0)
         {
            contours->setContours(contours->getContours()->v_next);
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
         else
         {
            Bool = false;
            total = 0;
         }
         
         (*(outputs[m_contoursOutID].buffer))[count] = ObjectRef(contours); 
         (*(outputs[m_boolID].buffer))[count] = ObjectRef(Bool::alloc(Bool));
         (*(outputs[m_nbElementsID].buffer))[count] = ObjectRef(Int::alloc(total));
         
      }
      
      NO_ORDER_NODE_SPEEDUP(CvNextHierarchy)
   };
}//namespace FD
