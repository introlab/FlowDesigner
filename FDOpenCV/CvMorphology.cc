#include "BufferedNode.h"
#include "operators.h"
#include "CvImage.h"
#include "CvStructuringElement.h"

using namespace std;

namespace FD {
   
   class CvMorphology;
   
   DECLARE_NODE(CvMorphology)
   /*Node
   *
   * @name CvMorphology
   * @category FDOpenCV:Morphological Operations
   * @description Performs advanced morphological transformations
   *
   * @input_name IMAGEIN
   * @input_description Source Image
   * @input_type CvImage
   *
   * @input_name ELEMENT
   * @input_description Structuring element
   * @input_type CvStructuringElement
   *
   * @parameter_name OPERATION
   * @parameter_type string
   * @parameter_value CV_MOP_OPEN;CV_MOP_CLOSE;CV_MOP_GRADIENT;CV_MOP_TOPHAT;CV_MOP_BLACKHAT
   * @parameter_description Type of morphological operation
   *
   * @parameter_name ITERATIONS
   * @parameter_type int
   * @parameter_value 1
   * @parameter_description Number of times erosion and dilation are applied
   * 
   * @output_name IMAGEOUT
   * @output_description Destination image
   * @output_type	CvImage
   * 
   END*/
   
   class CvMorphology : public BufferedNode {
      
      //Input ID
      int m_imageInID;
      int m_elementID;
      //Output ID
      int m_imageOutID;
      //Parameters
      int m_iterations;
      string m_operation;
      //map of the line_type
      map<string,int> m_operationMap;
      
      public:
      CvMorphology(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageInID = addInput("IMAGEIN");
         m_elementID = addInput("ELEMENT");
         
         //add outputs
         m_imageOutID = addOutput("IMAGEOUT");
         
         //Initialize parameters         
         m_iterations = dereference_cast<int>(parameters.get("ITERATIONS"));
         m_operation = object_cast<String>(parameters.get("OPERATION"));         
         
         //Initialize the lineType map
         m_operationMap["CV_MOP_OPEN"] = CV_MOP_OPEN;
         m_operationMap["CV_MOP_CLOSE"] = CV_MOP_CLOSE;
         m_operationMap["CV_MOP_GRADIENT"] = CV_MOP_GRADIENT;   
         m_operationMap["CV_MOP_TOPHAT"] = CV_MOP_TOPHAT;
         m_operationMap["CV_MOP_BLACKHAT"] = CV_MOP_BLACKHAT;          
      }
      
      void calculate(int output_id, int count, Buffer &out)
      {           
         //Read the inputs
         RCPtr<CvStructuringElement> elementPtr = getInput(m_elementID,count);
         RCPtr<CvImage> imagePtr = getInput(m_imageInID,count);
         
         //Handle
         
         //CvImage* imageTemp = imagePtr->zero();
         //CvImage* imageOut = imagePtr->zero();
         CvImage* imageTemp= new CvImage(imagePtr->getImage());  
         CvImage* imageOut= new CvImage(imagePtr->getImage()); 
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent );
         __BEGIN__; 
         OPENCV_CALL(cvMorphologyEx(imagePtr->getImage(), imageOut->getImage(), imageTemp->getImage(), elementPtr->getStructuringElement(), m_operationMap[m_operation], m_iterations)); 
         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to morphology the image: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         } 
         
         out[count] = ObjectRef(imageOut);
      }
      
      NO_ORDER_NODE_SPEEDUP(CvMorphology)
   };
}//namespace FD
