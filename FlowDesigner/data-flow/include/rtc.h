// Copyright (C) 2002 Jean-Marc Valin

#ifndef _RTC_H
#define _RTC_H

#include <pthread.h>
#include "pseudosem.h"
#include <list>

namespace FD {

class RTCTimer;

class RTCUser {
   pseudosem_t sem;
   float period;
   float next;
public:
   RTCUser(float p) : period(p) , next(p) {pseudosem_init(&sem,0,0);}
   ~RTCUser() {pseudosem_destroy(&sem);}
   void wait() {pseudosem_wait(&sem);}
   void wait(float d) {set(d); pseudosem_wait(&sem);}
protected:
   bool dec(float t) {next-=t;if (next>0) return false; else {next+=period;return true;}}
   void set(float d) {next=d;}
   void interrupt(float time) {while (dec(time)) {pseudosem_post(&sem);}}
   friend class RTCTimer;
};

class RTCTimer {
   int fd;
   pthread_t thread;
   pthread_mutex_t lock;
   std::list<RTCUser *> users;
   bool exit_status;
   int freq;
   float dt;
public:
   static RTCUser *create(float delay);
   static void destroy(RTCUser *u);
   //static int wait(int delay=0);
   //protected:
   RTCTimer();
   ~RTCTimer();
   static RTCTimer &instance();
   void runThread();
   friend class RTCUser;
};

}//namespace FD
#endif
