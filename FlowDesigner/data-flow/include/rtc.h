// Copyright (C) 2002 Jean-Marc Valin

#ifndef _RTC_H
#define _RTC_H

#include <pthread.h>
#include "pseudosem.h"
#include <list>

using namespace std;

class RTCTimer;

class RTCUser {
   pseudosem_t sem;
   int period;
   int next;
public:
   RTCUser(int p) : period(p) , next(p) {pseudosem_init(&sem,0,0);}
   ~RTCUser() {pseudosem_destroy(&sem);}
   void wait(int d=0) {if (d) set(d); pseudosem_wait(&sem);}
protected:
   bool dec() {next--;if (next) return false; else {next=period;return true;}}
   void set(int n) {next=n;}
   void interrupt() {if (dec()) pseudosem_post(&sem);}
   friend class RTCTimer;
};

class RTCTimer {
   int fd;
   pthread_t thread;
   pthread_mutex_t lock;
   list<RTCUser *> users;
   bool exit_status;
public:
   static RTCUser *create(int delay);
   static void destroy(RTCUser *u);
   static int wait(int delay=0);
   //protected:
   RTCTimer();
   ~RTCTimer();
   static RTCTimer &instance();
   void runThread();
   friend class RTCUser;
};

#endif
