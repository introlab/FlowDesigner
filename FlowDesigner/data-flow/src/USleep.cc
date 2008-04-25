#ifndef _USLEEP_CC_
#define _USLEEP_CC_

#include "BufferedNode.h"
#include "Timer.h"
#include <unistd.h>
#include <sys/time.h>
#include <unistd.h>

using namespace std;

namespace FD {
	
class USleep;

DECLARE_NODE(USleep)

/*Node

 * @name USleep
 * @category Time
 * @description Wait a given time.
 *
 * @parameter_name MICROSECONDS
 * @parameter_description The time required to sleep
 * @parameter_type int
 *
 * @output_name OUTPUT
 * @output_description Nothing
 *
END*/


class USleep : public BufferedNode {
  
  //outputs
  int outputID;

  //parameters
  int m_sleep_time;

  //local variables
  struct timespec m_tStartTime;

public:

   USleep(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     //inputs

     //outputs
     outputID = addOutput("OUTPUT");

     //parameters
     m_sleep_time = dereference_cast<int>(parameters.get("MICROSECONDS"));

     //getting starting time value
     vGetSystemTime( &m_tStartTime );
   }

   void calculate(int output_id, int count, Buffer &out) {

     //TO DO: Study the possibility to use Jean-Marc's driver for
     //		real time clock ("/dev/rtc") to do this

     //NOTE: This is to be used to calculate relatively short periods
     //		of time (max ~30 min)
     long l_lDiffTime;
     struct timespec l_tEndTime;

     //getting new time value
     vGetSystemTime( &l_tEndTime );
     //calculating time difference (microseconds)
     l_lDiffTime = lUsBetweenTimes( &m_tStartTime, &l_tEndTime );

     if( m_sleep_time < l_lDiffTime )
     {
       //cerr << "Processing taking too long by " << (l_lDiffTime - m_sleep_time) << " us." << endl;
       //getting new start time
       vGetSystemTime( &m_tStartTime );
     }
     //Makes the current thread sleep for some microseconds
     else
     {
       thread_usleep( m_sleep_time - l_lDiffTime );
       //getting new start time
       vAddUsToTime( m_sleep_time, &m_tStartTime );
     }


     out[count] = ObjectRef(Bool::alloc(true));


   }//calculate
  
};



}//namespace FD
#endif

