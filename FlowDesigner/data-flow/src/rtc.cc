// Copyright (C) 2002 Jean-Marc Valin

#include "rtc.h"
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <sys/ioctl.h>
#include <BaseException.h>

#if defined(HAVE_LINUX_RTC_H) || defined(HAVE_LINUX_MC146818RTC_H)
#define HAVE_RTC

#ifdef HAVE_LINUX_RTC_H
#include <linux/rtc.h>
#else
#include <linux/mc146818rtc.h>
#endif
#endif
//#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

RTCUser *RTCTimer::create(float delay)
{
   RTCUser *u = new RTCUser(delay);
   pthread_mutex_lock(&instance().lock);
   if (instance().users.empty())
   {
#ifdef HAVE_RTC
      int retval = ioctl(instance().fd, RTC_PIE_ON, 0);
      if (retval == -1) {
         perror("ioctl");
         //exit(errno);
      }
#endif
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
#ifdef HAVE_RTC
      int retval = ioctl(instance().fd, RTC_PIE_OFF, 0);
      if (retval == -1) {
         perror("ioctl");
         //exit(errno);
      }
#endif
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
#ifndef HAVE_RTC
   throw new GeneralException("Overflow not compiled with /dev/rtc support", __FILE__, __LINE__);
#endif
   pthread_mutex_init(&lock, NULL);
      pthread_mutex_lock(&lock);

   pthread_create(&thread, NULL, (void * (*)(void *)) start_rtc_thread, this);

}

RTCTimer::~RTCTimer()
{
   cerr << "Destroying timer..." << endl;
   exit_status=true;
#ifdef HAVE_RTC
   ioctl(fd, RTC_PIE_ON, 0);
#endif
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
   int retval;
#ifdef HAVE_RTC
   freq=16;
   do{
      freq*=2;
      retval = ioctl(fd, RTC_IRQP_SET, freq*2);
      /*if (retval == -1) {
        perror("ioctl");
        //exit(errno);
        }*/
      
   } while (retval!=-1);
   //cerr << "freq = " << freq << endl;
   dt=1.0/freq;
#endif
   //cerr << "dt = " << dt << endl;
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
         (*it)->interrupt(dt);
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
