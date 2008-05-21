// Copyright (C) 2001 InfoSpace Speech Solutions
// author: Jean-Marc Valin

#ifndef AUDIO_H
#define AUDIO_H

#include <math.h>

inline float bark(float f)
{
   return 13*atan(0.00076*f) + 3.5*atan((0.0001333*f) * (0.0001333*f));
}

inline float dB(float P)
{
   return 4.3429*log(P);
}

inline float undB(float d)
{
   return exp(0.23026*d);
}

#endif
