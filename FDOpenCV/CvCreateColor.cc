#include "BufferedNode.h"
#include "operators.h"
#include "CvColor.h"

using namespace std;

namespace FD {
   
   class CvCreateColor;
   
   DECLARE_NODE(CvCreateColor)
   /*Node
   *
   * @name CvCreateColor
   * @category FDOpenCV
   * @description Create a RGB colour
   *
   * @parameter_name CHANNEL1
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Red parameter
   *
   * @parameter_name CHANNEL2
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Green parameter
   *
   * @parameter_name CHANNEL3
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Blue parameter
   * 
   * @output_name COLOUR
   * @output_description The element structure
   * @output_type	CvColor
   * 
   END*/
   
   
   class CvCreateColor : public BufferedNode {
      //Output ID
      int m_colourID;
      //Parameters
      int m_channel1;
      int m_channel2;
      int m_channel3;
      
      public:
      CvCreateColor(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add outputs
         m_colourID = addOutput("COLOUR");
         //Initialize parameters         
         m_channel1 = dereference_cast<int>(parameters.get("CHANNEL1"));
         m_channel2 = dereference_cast<int>(parameters.get("CHANNEL2"));
         m_channel3 = dereference_cast<int>(parameters.get("CHANNEL3"));
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {   
         //Handle
         RCPtr<CvColor> color(new CvColor(m_channel1, m_channel2, m_channel3));  
         out[count] = color;
      }
      
      NO_ORDER_NODE_SPEEDUP(CvCreateColor)
   };
}//namespace FD