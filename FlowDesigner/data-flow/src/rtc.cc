// Copyright (C) 2002 Jean-Marc Valin

#include "rtc.h"
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/rtc.h>
//#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

RTCUser *RTCTimer::create(int delay)
{
   RTCUser *u = new RTCUser(delay);
   pthread_mutex_lock(&instance().lock);
   if (instance().users.empty())
   {
      int retval = ioctl(instance().fd, RTC_PIE_ON, 0);
      if (retval == -1) {
         perror("ioctl");
         //exit(errno);
      }

   }

   instance().users.push_front(u);
   pthread_mutex_unlock(&instance().lock);
   return u;
}

void RTCTimer::destroy(RTCUser *u)
{
   pthread_mutex_lock(&instance().lock);

   list<RTCUser*>::iterator it=instance().users.begin();
   while (it != instance().users.end())
   {
      if (*it==u)
      {
         instance().users.erase(it);
         break;
      }
      it++;
   }
   
   if (instance().users.empty())
   {
      int retval = ioctl(instance().fd, RTC_PIE_OFF, 0);
      if (retval == -1) {
         perror("ioctl");
         //exit(errno);
      }

   }
   
   pthread_mutex_unlock(&instance().lock);
   delete u;
}

static void start_rtc_thread(RTCTimer *t)
{
   t->runThread();
}

RTCTimer::RTCTimer()
   : exit_status(false)
{
   pthread_mutex_init(&lock, NULL);
      pthread_mutex_lock(&lock);

   pthread_create(&thread, NULL, (void * (*)(void *)) start_rtc_thread, this);

}

RTCTimer::~RTCTimer()
{
   cerr << "Destroying timer..." << endl;
   exit_status=true;
   pthread_join(thread, NULL);
   cerr << "Done" << endl;
}

void RTCTimer::runThread()
{
   fd = open("/dev/rtc", O_RDONLY);
   if (fd<0)
   {
      perror("/dev/rtc");
      cerr << "Cannot open /dev/rtc" << endl;
   }
   /*int retval = ioctl(fd, RTC_PIE_ON, 0);
   if (retval == -1) {
      perror("ioctl");
      //exit(errno);
      }
   */
   int retval = ioctl(fd, RTC_IRQP_SET, 64);
   if (retval == -1) {
      perror("ioctl");
      //exit(errno);
   }
   pthread_mutex_unlock(&lock);

   while(1)
   {
      unsigned long ret;
      ret = read(fd,&ret,4);
      if (exit_status)
         pthread_exit(NULL);
      pthread_mutex_lock(&lock);
      
      //cerr << "+";
      //cerr.flush();
      list<RTCUser*>::iterator it=users.begin();
      while (it != users.end())
      {
         (*it)->interrupt();
         it++;
      }

      pthread_mutex_unlock(&lock);
   }
}

RTCTimer &RTCTimer::instance()
{
   static RTCTimer ret;
   return ret;
}
