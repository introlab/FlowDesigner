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
 * @parameter_name MICROSECONDS
 * @parameter_description Sleep x microseconds.
 * @parameter_type int
 *
END*/

class Sleep : public Node {
  
private:

  int outputID;
  int m_time;
   RTCUser *rtc;
public:

  Sleep(string nodeName, ParameterSet params) 
      : Node(nodeName, params) {

    m_time = dereference_cast<int>(parameters.get("MICROSECONDS"));

    outputID = addOutput("VALUE");
    int tmp = (64*m_time/1000000);
    if (!tmp)
       tmp=1;
    //cerr << "time = " << tmp << endl;
    rtc=RTCTimer::create(tmp);
  }
    
   ~Sleep() {RTCTimer::destroy(rtc);}

  void specificInitialize() {
    this->Node::specificInitialize();
  }
  
  void reset() {
    this->Node::reset();
  }

  ObjectRef getOutput(int output_id, int count) {

    //sleeping
    //usleep(m_time);
     rtc->wait();
    if (output_id==outputID) return TrueObject;
    else throw new NodeException (this, "Sleep: Unknown output id", __FILE__, __LINE__);
  }
};
#endif
