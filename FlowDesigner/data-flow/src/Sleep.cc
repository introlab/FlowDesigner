// Copyright (C) 2001 Dominic Letourneau

#ifndef _SLEEP_CC_
#define _SLEEP_CC_

#include "Node.h"
#include "Object.h"
#include "rtc.h"
//#include <unistd.h>

class Sleep;

DECLARE_NODE(Sleep)
/*Node
 *
 * @name Sleep
 * @category Flow
 * @description Sleep a certain amount of time.
 *
 * @output_name VALUE
 * @output_description Always return TRUE.
 *
 * @parameter_name SECONDS
 * @parameter_description Sleep x seconds.
 * @parameter_type float
 *
END*/

class Sleep : public Node {
  
private:

  int outputID;
  float m_time;
   RTCUser *rtc;
public:

   Sleep(string nodeName, ParameterSet params) 
      : Node(nodeName, params) 
   {
      
      m_time = dereference_cast<float>(parameters.get("SECONDS"));
      
      outputID = addOutput("VALUE");
      rtc=RTCTimer::create(m_time);
   }
   
   ~Sleep() {RTCTimer::destroy(rtc);}
   
   ObjectRef getOutput(int output_id, int count) 
   {      
      //sleeping
      //usleep(m_time);
      rtc->wait();
      if (output_id==outputID) return TrueObject;
      else throw new NodeException (this, "Sleep: Unknown output id", __FILE__, __LINE__);
   }
};
#endif
