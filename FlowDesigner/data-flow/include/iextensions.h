// Copyright (C) 2001 InfoSpace Speech Solutions
// author: Jean-Marc Valin

#ifndef IEXTENSIONS_H
#define IEXTENSIONS_H

class IExtensions {
   static bool isse;
   static bool i3dnow;
  public:
   static void detect();
   static void detectSSE();
   static void detect3DNow();
   static inline bool haveSSE() {return isse;}
   static inline bool have3DNow() {return i3dnow;}
   
};

#endif
