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
   * @category FDOpenCV:Contours
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
   * CV_RETR_EXTERNAL: retrive only the extreme outer contours
   * CV_RETR_LIST: retrieve all the contours and puts them in the list
   * CV_RETR_CCOMP: retrieve all the contours and organizes them into two-level hierarchy: top level are external boundaries of the components, second level are bounda boundaries of the holes
   * CV_RETR_TREE: retrieve all the contours and reconstructs the full hierarchy of nested contours
   *
   * @parameter_name METHOD
   * @parameter_type string
   * @parameter_value CV_CHAIN_APPROX_NONE;CV_CHAIN_APPROX_SIMPLE;CV_CHAIN_APPROX_TC89_L1;CV_LINK_RUNS
   * @parameter_description Approximation method
   * CV_CHAIN_CODE: output contours in the Freeman chain code. All other methods output polygons (sequences of vertices).
   * CV_CHAIN_APPROX_NONE: translate all the points from the chain code into points;
   * CV_CHAIN_APPROX_SIMPLE: compress horizontal, vertical, and diagonal segments, that is, the function leaves only their ending points;
   * CV_CHAIN_APPROX_TC89_L1,CV_CHAIN_APPROX_TC89_KCOS: apply one of the flavors of Teh-Chin chain approximation algorithm.
   * CV_LINK_RUNS: use completely different contour retrieval algorithm via linking of horizontal segments of 1?s. Only CV_RETR_LIST retrieval mode can be used with this method.
   *
   * @parameter_name AREA_MIN
   * @parameter_type float
   * @parameter_value 0
   * @parameter_description All the contours which are superior to this value will be retrived.
   *
   * @output_name CONTOURS
   * @output_description Contain the pointer to the first outer contour
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
      float m_areaMin;
      
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
         m_areaMin = object_cast<Float>(parameters.get("AREA_MIN"));
         
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
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent ); 
         __BEGIN__;
         OPENCV_CALL(m_storage = cvCreateMemStorage(0));
         __END__;
         cvSetErrMode( status );       
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to find contours: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }         
      }
      
      ~CvFindContours()
      {
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent ); 
         __BEGIN__;
         OPENCV_CALL(cvReleaseMemStorage(&m_storage));
         __END__;
         cvSetErrMode( status );         
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to find contours: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }         
      }
      
      
      void calculate(int output_id, int count, Buffer &out)
      {   
         //Read the inputs
         RCPtr<CvImage> imagePtr = getInput(m_imageID,count);
         //Handle
         CvImage* image = new CvImage(imagePtr->getImage());
         CvSeq* contours; 
         int nbContours=0;
         double area;
         CvContourScanner scanner;
         
         int status = cvGetErrMode();
         cvSetErrMode( CV_ErrModeSilent ); 
         __BEGIN__;
         OPENCV_CALL(cvClearMemStorage( m_storage ));  
         OPENCV_CALL(scanner = cvStartFindContours( image->getImage(), m_storage
            , sizeof(CvContour)
            , m_modeMap[m_mode]
            , m_methodMap[m_method]
            , cvPoint(0,0) ));
         OPENCV_CALL(contours = cvFindNextContour( scanner ));
         while(contours!=NULL)
         {
            OPENCV_CALL(area = fabs(cvContourArea( contours )));
            if( area > m_areaMin)
            {
               OPENCV_CALL(cvSubstituteContour( scanner, contours ));
               nbContours++;
            }
            else
            {
               OPENCV_CALL(cvSubstituteContour( scanner, NULL));
            }
            OPENCV_CALL(contours = cvFindNextContour( scanner ));
         }
         contours = cvEndFindContours( &scanner );

         __END__;
         cvSetErrMode( status );
         if( cvGetErrStatus() != CV_StsOk  )
         {
            throw new GeneralException("OPENCV - Error to find contours: " +  CCHAR(cvErrorStr( cvGetErrStatus() )),__FILE__,__LINE__);
         }         
         
         CvContours* Contours = new CvContours();
         Contours->setNbContours(nbContours);
         Contours->setContours(contours);
         Contours->setFirstContours(contours);
         (*(outputs[m_contoursID].buffer))[count] = ObjectRef(Contours); 
         (*(outputs[m_nbContoursID].buffer))[count] = ObjectRef(Int::alloc(nbContours));
      }
      
      NO_ORDER_NODE_SPEEDUP(CvFindContours)
   };
}//namespace FD
