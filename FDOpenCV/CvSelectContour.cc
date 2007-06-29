#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"
#include "CvImage.h"
#include "CvColor.h"

using namespace std;

namespace FD {
   
   class CvSelectContour;
   
   DECLARE_NODE(CvSelectContour)
   /*Node
   *
   * @name CvSelectContour
   * @category FDOpenCV:Contours
   * @description Point to the contour indicate by the index
   *
   * @input_name CONTOURSIN
   * @input_description The contour to handle
   * @input_type CvContours
   *
   * @input_name INDEX
   * @input_description index of the contour
   * @input_type int
   *
   * @output_name CONTOURSOUT
   * @output_description The result contour
   * @output_type	CvContours
   *
   * @output_name NBELEMENTS
   * @output_description Number of elemtents in the selected contour
   * @output_type	int
   * 
   * @output_name BOOL
   * @output_description true if the next contour exist. If false, the first contour is return
   * @output_type	bool   
   *   
   END*/
   inline std::string CINT(int value)
   {
      std::stringstream sstr;
      sstr << value;
      return(sstr.str());
   }   
   
   class CvSelectContour : public BufferedNode {
      
      //Input ID
      int m_contoursInID;
      int m_indexID;
      //Output ID
      int m_contoursOutID;
      int m_nbElementsID;
      int m_boolID;
      
      public:
      CvSelectContour(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_contoursInID = addInput("CONTOURSIN");
         m_indexID = addInput("INDEX");
         
         //add outputs
         m_contoursOutID = addOutput("CONTOURSOUT");
         m_nbElementsID = addOutput("NBELEMENTS");
         m_boolID = addOutput("BOOL");
         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvContours> contoursPtr = getInput(m_contoursInID,count);
         RCPtr<Int> indexPtr = getInput(m_indexID,count);
         
         //Handle of the inputs
         
         int i=0;
         bool Bool = true;
         CvContours* contours = new CvContours(&(*contoursPtr));
         contours->setContours(contours->getFirstContours()->h_next);
         for(i=0; i<*(indexPtr); i++)
         {
            if((contours->getContours()) == 0)
            {
               throw new GeneralException("OPENCV - Contour doesn't exist",__FILE__,__LINE__);
            }
            contours->setContours(contours->getContours()->h_next);
         }
         
         if((contours->getContours()) == 0)
         {
            Bool = false;
            contours->setContours(contours->getFirstContours());
         }     
         (*(outputs[m_boolID].buffer))[count] = ObjectRef(Bool::alloc(Bool));
         (*(outputs[m_contoursOutID].buffer))[count] = ObjectRef(contours); 
         (*(outputs[m_nbElementsID].buffer))[count] = ObjectRef(Int::alloc( ((contours->getContours())->total)));
      }
      
      NO_ORDER_NODE_SPEEDUP(CvSelectContour)
   };
}//namespace FD
