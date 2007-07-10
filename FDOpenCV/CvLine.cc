#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include "CvColor.h"
#include <math.h>

using namespace std;

namespace FD {
   
   class CvLine;
   
   DECLARE_NODE(CvLine)
   /*Node
   *
   * @name CvLine
   * @category FDOpenCV:Draw
   * @description Draws contour outlines or interiors in the image
   *
   * @input_name IMAGEIN
   * @input_description the input image
   * @input_type CvImage
   *
   * @input_name COLOR
   * @input_description the BGR color of the rectangle
   * @input_type CvColor
   *
   * @input_name X1
   * @input_description x coordinate to the first extremity of the line
   * @input_type int
   *
   * @input_name Y1
   * @input_description y coordinate to the first extremity of the line
   * @input_type int
   *
   * @input_name X2
   * @input_description x coordinate to the second extremity of the line
   * @input_type int
   *
   * @input_name Y2
   * @input_description y coordinate to the second extremity of the line
   * @input_type int
   *
   * @parameter_name THICKNESS
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Thickness of lines that make up the rectangle. Negative values, make the function to draw a filled rectangle
   *   
   * @parameter_name LINE_TYPE
   * @parameter_type string
   * @parameter_value 8;4;CV_AA
   * @parameter_description Type of the contour segments
   *   
   * @parameter_name SHIFT
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Number of fractional bits in the point coordinates
   * 
   * @output_name IMAGEOUT
   * @output_description The result image
   * @output_type	CvImage
   * 
   END*/
   inline std::string CINT(int value)
   {
      std::stringstream sstr;
      sstr << value;
      return(sstr.str());
   }   
   
   class CvLine : public BufferedNode {
      
      //Input ID
      int m_colorID;
      int m_imageInID;
      int m_x1ID;
      int m_y1ID;
      int m_x2ID;
      int m_y2ID; 
      //Output ID
      int m_imageOutID;
      //Parameter
      string m_lineType;
      int m_thickness;
      int m_shift; 
      //map of the line_type
      map<string,int> m_lineTypeMap;
      
      public:
      CvLine(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_colorID = addInput("COLOR");
         m_imageInID = addInput("IMAGEIN");
         m_x1ID = addInput("X1");
         m_y1ID = addInput("Y1");
         m_x2ID = addInput("X2");
         m_y2ID = addInput("Y2");         
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         
         //Initialize parameters         
         m_thickness = dereference_cast<int>(parameters.get("THICKNESS"));
         m_shift = dereference_cast<int>(parameters.get("SHIFT"));         
         m_lineType = object_cast<String>(parameters.get("LINE_TYPE"));         
         
         //Initialize lineType map
         m_lineTypeMap["8"] = 8;
         m_lineTypeMap["4"] = 4;
         m_lineTypeMap["CV_AA"] = CV_AA;         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvColor> colorPtr = getInput(m_colorID,count);
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);
         RCPtr<Int> x1Ptr = getInput(m_x1ID,count);
         RCPtr<Int> y1Ptr = getInput(m_y1ID,count);
         RCPtr<Float> x2Ptr = getInput(m_x2ID,count);
         RCPtr<Float> y2Ptr = getInput(m_y2ID,count);         
         //Handle
         CvImage* image = new CvImage(&(*imagePtr));
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );         
         __BEGIN__; 
         OPENCV_CALL( cvLine( image->getImage(), cvPoint(*x1Ptr,*y1Ptr)
            , cvPoint(*x2Ptr,*y2Ptr), CV_RGB( (colorPtr->getColor()).val[0], (colorPtr->getColor()).val[1], (colorPtr->getColor()).val[2] )
            , m_thickness ,m_lineTypeMap[m_lineType], m_shift ));         
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to draw a line: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvLine)
   };
}//namespace FD
