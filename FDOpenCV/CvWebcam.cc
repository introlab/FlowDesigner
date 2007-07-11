#include "BufferedNode.h"
#include "operators.h"
#include "CvVideo.h"
#include "CvImage.h"

using namespace std;

namespace FD {
   
   using namespace std;
   
   class CvWebcam;
   
   DECLARE_NODE(CvWebcam)
   /*Node
   *
   * @name CvWebcam
   * @category FDOpenCV:Video
   * @description Load a video from a file
   *
   * @parameter_name INDEX
   * @parameter_type int
   * @parameter_value -1
   * @parameter_description Index of the camera to be used. If there is only one camera or it does not matter what camera to use -1 may be passed.
   * 
   * @output_name VIDEO
   * @output_description The loaded video data structure
   * @output_type	CvVideo
   * 
   END*/
   
   
   class CvWebcam : public BufferedNode {
      
      //Output ID
      int m_videoID;
      //Parameters
      int m_index;      
      
      public:
      CvWebcam(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      {
         //add outputs
         m_videoID = addOutput("VIDEO");
         //Initialize parameters         
         m_index = dereference_cast<int>(parameters.get("INDEX"));
      }
      
      
      void calculate(int output_id, int count, Buffer &out)
      {
         
         //Handle
         RCPtr<CvVideo> videoPtr(new CvVideo(m_index));       
         out[count] = videoPtr;
      }
      
      NO_ORDER_NODE_SPEEDUP(CvWebcam)
   };
}//namespace FD
