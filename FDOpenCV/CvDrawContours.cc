#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"
#include "CvImage.h"
#include "CvColor.h"

using namespace std;

namespace FD {
   
   class CvDrawContours;
   
   DECLARE_NODE(CvDrawContours)
   /*Node
   *
   * @name CvDrawContours
   * @category FDOpenCV:contour
   * @description Draws contour outlines or interiors in the image
   *
   * @input_name CONTOURSIN
   * @input_description The contours to handle
   * @input_type CvContours
   *
   * @input_name IMAGEIN
   * @input_description the input image
   * @input_type CvImage
   *
   * @input_name EXTERNAL_COLOR
   * @input_description Color of the external contours
   * @input_type CvColor
   *
   * @input_name HOLE_COLOR
   * @input_description Color of internal contours (holes)
   * @input_type CvColor
   *
   * @parameter_name MAX_LEVEL
   * @parameter_type int
   * @parameter_value 0
   * @parameter_description Maximal level for drawn contours
   *
   * @parameter_name THICKNESS
   * @parameter_type int
   * @parameter_value -1
   * @parameter_description Thickness of lines the contours are drawn with. If it is negative, the contour interiors are drawn
   *
   * @parameter_name LINE_TYPE
   * @parameter_type string
   * @parameter_value 8;4;CV_AA
   * @parameter_description Type of the contour segments
   * 
   * @output_name IMAGEOUT
   * @output_description The result image
   * @output_type	CvImage
   *
   END*/
   
   class CvDrawContours : public BufferedNode {
      
      //Input ID
      int m_contoursInID;
      int m_externalColorID;
      int m_holeColorID;
      int m_imageInID;
      //Output ID
      int m_imageOutID;
      //Parameters
      int m_maxLevel;
      int m_thickness;
      string m_lineType;
      //map of the line_type
      map<string,int> m_lineTypeMap;
      
      public:
      CvDrawContours(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_contoursInID = addInput("CONTOURSIN");
         m_externalColorID = addInput("EXTERNAL_COLOR");
         m_holeColorID = addInput("HOLE_COLOR");
         m_imageInID = addInput("IMAGEIN");
         
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         //Initialize parameters         
         m_maxLevel = dereference_cast<int>(parameters.get("MAX_LEVEL"));
         m_thickness = dereference_cast<int>(parameters.get("THICKNESS"));
         m_lineType = object_cast<String>(parameters.get("LINE_TYPE"));         
         
         //Initialize the lineType map
         m_lineTypeMap["8"] = 8;
         m_lineTypeMap["4"] = 4;
         m_lineTypeMap["CV_AA"] = CV_AA;         
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvContours> contoursPtr = getInput(m_contoursInID,count);
         RCPtr<CvColor> externalColorPtr = getInput(m_externalColorID,count);
         RCPtr<CvColor> holeColorPtr = getInput(m_holeColorID,count);
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);
         
         //Handle

         CvImage* image = new CvImage( imagePtr->getImage());      
         CvContours* contours = new CvContours( &(*contoursPtr));         

         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL( cvDrawContours( image->getImage(), contours->getContours()
            , externalColorPtr->getColor(), holeColorPtr->getColor()
            , m_maxLevel,m_thickness, m_lineTypeMap[m_lineType] )); 
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to draw the contours: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         (*(outputs[m_imageOutID].buffer))[count] = ObjectRef(image); 
      }
      
      NO_ORDER_NODE_SPEEDUP(CvDrawContours)
   };
}//namespace FD
