#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include "CvColor.h"

using namespace std;

namespace FD {
   
   class CvRectangle;
   
   DECLARE_NODE(CvRectangle)
   /*Node
   *
   * @name CvRectangle
   * @category FDOpenCV:Draw
   * @description Draws a rectangle in the image
   *
   * @input_name IMAGEIN
   * @input_description the input image
   * @input_type CvImage
   *
   * @input_name COLOR
   * @input_description the BGR color of the rectangle
   * @input_type CvColor
   * 
   * @parameter_name X1
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description x of the rectangle vertices   
   *
   * @parameter_name Y1
   * @parameter_type int
   * @parameter_value -1
   * @parameter_description y of the rectangle vertices
   *
   * @parameter_name X2
   * @parameter_type int
   * @parameter_value -1
   * @parameter_description x opposite rectangle vertex
   *   
   * @parameter_name Y2
   * @parameter_type int
   * @parameter_value -1
   * @parameter_description y opposite rectangle vertex   
   *
   * @parameter_name THICKNESS
   * @parameter_type int
   * @parameter_value -1
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
   
   class CvRectangle : public BufferedNode {
      
      //Input ID
      int m_colorID;
      int m_imageInID;
      //Output ID
      int m_imageOutID;
      //Parameter
      int m_x1;
      int m_y1;
      int m_x2;
      int m_y2;      
      string m_lineType;
      int m_thickness;
      int m_shift; 
      //map of the line_type
      map<string,int> m_lineTypeMap;
      
      public:
      CvRectangle(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_colorID = addInput("COLOR");
         m_imageInID = addInput("IMAGEIN");
         
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         
         //Initialize parameters         
         m_x1 = dereference_cast<int>(parameters.get("X1"));
         m_y1 = dereference_cast<int>(parameters.get("Y1"));
         m_x2 = dereference_cast<int>(parameters.get("X2"));
         m_y2 = dereference_cast<int>(parameters.get("Y2"));
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
         
         //Handle
         CvImage* image = new CvImage(imagePtr->getImage());
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL( cvRectangle( image->getImage(), cvPoint(m_x1,m_y1)
            , cvPoint(m_x2,m_y2), CV_RGB( (colorPtr->getColor()).val[0], (colorPtr->getColor()).val[1], (colorPtr->getColor()).val[2] )
            , m_thickness ,m_lineTypeMap[m_lineType], m_shift ));         
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to draw a rectangle: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         
         out[count] = ObjectRef(image);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvRectangle)
   };
}//namespace FD
