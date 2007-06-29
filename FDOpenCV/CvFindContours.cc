#include "BufferedNode.h"
#include "operators.h"
#include "CvContours.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   class CvFindContours;
   
   DECLARE_NODE(CvFindContours)
   /*Node
   *
   * @name CvFindContours
   * @category FDOpenCV:contour
   * @description Finds contours in binary image
   *
   * @input_name IMAGE
   * @input_description Input image
   * @input_type CvImage
   *
   * @parameter_name MODE
   * @parameter_type string
   * @parameter_value CV_RETR_EXTERNAL;CV_RETR_LIST;CV_RETR_CCOMP;CV_RETR_TREE
   * @parameter_description Retrieval mode
   *
   * @parameter_name METHOD
   * @parameter_type string
   * @parameter_value CV_CHAIN_CODE;CV_CHAIN_APPROX_NONE;CV_CHAIN_APPROX_SIMPLE;CV_CHAIN_APPROX_TC89_L1;CV_LINK_RUNS
   * @parameter_description Approximation method
   * 
   * @output_name CONTOURS
   * @output_description The element structure
   * @output_type	CvContours
   * 
   * @output_name NBCONTOURS
   * @output_description Number of contours
   * @output_type	int
   * 
   END*/
   
   
   class CvFindContours : public BufferedNode {
      //Input ID
      int m_imageID;
      //Outputs ID
      int m_contoursID;
      int m_nbContoursID;
      //Parameters
      string m_mode;
      string m_method;
      
      //map of the mode and methode
      map<string,int> m_modeMap;
      map<string,int> m_methodMap;
      
      //Memory Storage for the contours
      CvMemStorage* m_storage;
      
      public:
      CvFindContours(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_imageID = addInput("IMAGE");
         //add outputs
         m_contoursID = addOutput("CONTOURS");
         m_nbContoursID = addOutput("NBCONTOURS");
         //Initialize parameters         
         m_mode = object_cast<String>(parameters.get("MODE"));
         m_method = object_cast<String>(parameters.get("METHOD"));
         //Initialize the mode map
         m_modeMap["CV_RETR_EXTERNAL"] = CV_RETR_EXTERNAL;
         m_modeMap["CV_RETR_LIST"] = CV_RETR_LIST;
         m_modeMap["CV_RETR_CCOMP"] = CV_RETR_CCOMP;
         m_modeMap["CV_RETR_TREE"] = CV_RETR_TREE;
         //Initialize the method map
         m_methodMap["CV_CHAIN_CODE"] = CV_CHAIN_CODE;
         m_methodMap["CV_CHAIN_APPROX_NONE"] = CV_CHAIN_APPROX_NONE;
         m_methodMap["CV_CHAIN_APPROX_SIMPLE"] = CV_CHAIN_APPROX_SIMPLE;
         m_methodMap["CV_CHAIN_APPROX_TC89_L1"] = CV_CHAIN_APPROX_TC89_L1;
         m_methodMap["CV_LINK_RUNS"] = CV_LINK_RUNS;
         //Initialze the memory storage
         m_storage = cvCreateMemStorage(0);
      }
      
      ~CvFindContours()
      {
         cout << "Erase Storage" << endl;
         cvReleaseMemStorage(&m_storage);
      }
      
      
      void calculate(int output_id, int count, Buffer &out)
      {   
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageID,count);
         
         //Handle
         CvImage* image = new CvImage(imagePtr->getImage());
         CvContours* contours = new CvContours();
         CvSeq* firstContours; 
         int nbContours;
         cvSetErrMode( CV_ErrModeSilent ); 
         __BEGIN__;
         OPENCV_CALL(nbContours = cvFindContours( image->getImage(), m_storage
            , &firstContours, sizeof(CvContour)
            , m_modeMap[m_mode], m_methodMap[m_method] ));
         __END__;
         cvSetErrMode( CV_ErrModeLeaf );
         
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to find contours: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }         
         contours->setNbContours(nbContours);
         contours->setContours(firstContours);
         contours->setFirstContours(firstContours);
         (*(outputs[m_contoursID].buffer))[count] = ObjectRef(contours); 
         (*(outputs[m_nbContoursID].buffer))[count] = ObjectRef(Int::alloc(nbContours));
      }
      
      NO_ORDER_NODE_SPEEDUP(CvFindContours)
   };
}//namespace FD
