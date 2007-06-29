#include "BufferedNode.h"
#include "operators.h"
#include "CvStructuringElement.h"

using namespace std;

namespace FD {
   
   class CvCreateStructuringElement;
   
   DECLARE_NODE(CvCreateStructuringElement)
   /*Node
   *
   * @name CvCreateStructuringElement
   * @category FDOpenCV
   * @description Create A Structuring Element FROM PARAMETERS
   *
   * @parameter_name COLS
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description Number of columns in the structuring element
   *
   * @parameter_name ROWS
   * @parameter_type int
   * @parameter_value 3
   * @parameter_description Number of rows in the structuring element
   *
   * @parameter_name ANCHOR_X
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Relative horizontal offset of the anchor point
   *
   * @parameter_name ANCHORY_y
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Relative vertical offset of the anchor point 
   *
   * @parameter_name SHAPE
   * @parameter_type string
   * @parameter_value CV_SHAPE_RECT;CV_SHAPE_CROSS;CV_SHAPE_ELLIPSE;CV_SHAPE_CUSTOM;
   * @parameter_description Shape of the structuring element      
   *   
   * @output_name STRUCTURINGELEMENT
   * @output_description The element structure
   * @output_type	CvStructuringElement
   * 
   END*/
   
   
   class CvCreateStructuringElement : public BufferedNode {
      
      //Output ID
      int m_structuringElementID;
      //Parameter
      int m_cols;
      int m_rows;
      int m_anchor_x;
      int m_anchor_y;
      string m_shape;
      //map of the shape
      map<string,int> m_shapeMap;
      
      public:
      CvCreateStructuringElement(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add outputs
         m_structuringElementID = addOutput("STRUCTURINGELEMENT");
         
         //Initialize parameters         
         m_cols = dereference_cast<int>(parameters.get("COLS"));
         m_rows = dereference_cast<int>(parameters.get("ROWS"));
         m_anchor_x = dereference_cast<int>(parameters.get("ANCHOR_X"));
         m_anchor_y = dereference_cast<int>(parameters.get("ANCHORY_y"));
         m_shape = object_cast<String>(parameters.get("SHAPE"));
         
         //Initialize the shape map
         m_shapeMap["CV_SHAPE_RECT"] = CV_SHAPE_RECT;
         m_shapeMap["CV_SHAPE_CROSS"] = CV_SHAPE_CROSS;
         m_shapeMap["CV_SHAPE_ELLIPSE"] = CV_SHAPE_ELLIPSE; 
         m_shapeMap["CV_SHAPE_CUSTOM"] = CV_SHAPE_CUSTOM;
         
      }
            
      void calculate(int output_id, int count, Buffer &out)
      {   
         //Handle
         RCPtr<CvStructuringElement> structuringElementPtr(new CvStructuringElement(m_cols, m_rows, m_anchor_x, m_anchor_y, m_shapeMap[m_shape]));  
         out[count] = structuringElementPtr;
      }
      
      NO_ORDER_NODE_SPEEDUP(CvCreateStructuringElement)
   };
}//namespace FD
