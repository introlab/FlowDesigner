#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"

using namespace std;

namespace FD {
   
   class CvPrevHierarchy;
   
   DECLARE_NODE(CvPrevHierarchy)
   /*Node
   *
   * @name CvPrevHierarchy
   * @category FDOpenCV:contour
   * @description Point to the next contour
   *
   * @input_name CONTOURSIN
   * @input_description The contour to handle
   * @input_type CvContours
   * 
   * @output_name CONTOURSOUT
   * @output_description The result contour
   * @output_type	CvContours
   *
   * @output_name BOOL
   * @output_description True if the next contour exist
   * @output_type	bool   
   *
   * @output_name NBELEMENTS
   * @output_description Number of elements in the selected contour
   * @output_type	int
   * 
   END*/
   
   class CvPrevHierarchy : public BufferedNode {
      
      //Input ID
      int m_contoursInID;
      
      //Output ID
      int m_contoursOutID;
      int m_nbElementsID;
      int m_boolID;
      
      
      public:
      CvPrevHierarchy(string nodeName, ParameterSet params)
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
         if((contoursPtr->getContours()) == 0)
         {
            throw new GeneralException("OPENCV - Contour doesn't exist",__FILE__,__LINE__);
         } 
         CvContours* contours = new CvContours(&(*contoursPtr));         
         contours->setContours(contours->getContours()->v_prev);
         if((contours->getContours()) == 0)
         {
            Bool = false;
            contours->setContours(contours->getFirstContours());
         }          
         (*(outputs[m_contoursOutID].buffer))[count] = ObjectRef(contours); 
         (*(outputs[m_boolID].buffer))[count] = ObjectRef(Bool::alloc(Bool));
         (*(outputs[m_nbElementsID].buffer))[count] = ObjectRef(Int::alloc( ((contours->getContours())->total)));
         
      }
      
      NO_ORDER_NODE_SPEEDUP(CvPrevHierarchy)
   };
}//namespace FD
