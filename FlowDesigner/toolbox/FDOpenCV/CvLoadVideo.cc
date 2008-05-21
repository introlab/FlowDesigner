#include "BufferedNode.h"
#include "operators.h"
#include "CvVideo.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   using namespace std;
   
   class CvLoadVideo;
   
   DECLARE_NODE(CvLoadVideo)
   /*Node
   *
   * @name CvLoadVideo
   * @category FDOpenCV:Video
   * @description Load a video from a file
   *
   * @input_name FILENAME
   * @input_description The file name to load
   * @input_type string
   *
   * @output_name VIDEO
   * @output_description The loaded video data structure
   * @output_type CvVideo
   *
   END*/
   
   
   class CvLoadVideo : public BufferedNode {
      //Input ID
      int m_filenameID;
      //Output ID
      int m_videoID;
      
      public:
      CvLoadVideo(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add inputs
         m_filenameID = addInput("FILENAME");
         //add outputs
         m_videoID = addOutput("VIDEO");
      }
      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         //Read the inputs
         RCPtr<String> filename = getInput(m_filenameID,count); 
         
         //Handle
         RCPtr<CvVideo> videoPtr(new CvVideo(*filename));       
         out[count] = videoPtr;
      }
      
      NO_ORDER_NODE_SPEEDUP(CvLoadVideo)
   };
}//namespace FD
