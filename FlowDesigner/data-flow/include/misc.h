// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef MISC_H
#define MISC_H

/**Max function*/
template <class T>
T &max(T &a, T &b) {return a > b ? a : b;}

/**Min function*/
template <class T>
T &min(T &a, T &b) {return a < b ? a : b;}

/**Square function*/
template <class T>
inline T sqr(T x) {return x*x;}

/**Absolute value function*/
template <class T>
inline T abs(T x) {return x >= 0 ? x : -x;}

/**Norm of a vector*/
template <class T>
inline T vec_norm(T *x, int len)
{
   T sum1=0,sum2=0,sum3=0,sum4=0;
   while (len > 3)
   {
      sum1 += *x* *x;
      x++;
      sum2 += *x* *x;
      x++;
      sum3 += *x* *x;
      x++;
      sum4 += *x* *x;
      x++;
      len -= 4;
   }
   while (len)
   {
      sum1 += *x* *x;
      x++;
      len--;
   }

   return sqrt((sum1+sum2)+(sum3+sum4));
}

/**Squared norm of a vector*/
template <class T>
inline T vec_norm2(T *x, int len)
{
   T sum1=0,sum2=0,sum3=0,sum4=0;
   while (len > 3)
   {
      sum1 += *x* *x;
      x++;
      sum2 += *x* *x;
      x++;
      sum3 += *x* *x;
      x++;
      sum4 += *x* *x;
      x++;
      len -= 4;
   }
   while (len)
   {
      sum1 += *x* *x;
      x++;
      len--;
   }

   return (sum1+sum2)+(sum3+sum4);
}

/**dot product of two vector*/
template <class T>
inline T vec_dot_prod(T *x, T *y, int len)
{
   T sum1=0,sum2=0,sum3=0,sum4=0;
   while (len > 3)
   {
      sum1 += *x++* *y++;
      sum2 += *x++* *y++;
      sum3 += *x++* *y++;
      sum4 += *x++* *y++;
      len -= 4;
   }
   while (len)
   {
      sum1 += *x++* *y++;
      len--;
   }

   return (sum1+sum2)+(sum3+sum4);
}

/**vector product by a scalar*/
template <class T>
inline void vec_prod_scalar(T *x, T a, T *y, int len)
{
   while (len > 3)
   {
      *y++ = *x++ * a;
      *y++ = *x++ * a;
      *y++ = *x++ * a;
      *y++ = *x++ * a;
      len -= 4;
   }
   while (len)
   {
      *y++ = *x++ * a;
      len--;
   }
}

/**vector add*/
template <class T>
inline void vec_add(T *x1, T *x2, T *y, int len)
{
   while (len > 3)
   {
      *y++ = *x1++ + *x2++;
      *y++ = *x1++ + *x2++;
      *y++ = *x1++ + *x2++;
      *y++ = *x1++ + *x2++;
      len -= 4;
   }
   while (len)
   {
      *y++ = *x1++ + *x2++;
      len--;
   }
}

/**vector copy*/
template <class T>
inline void vec_copy(T *x, T *y, int len)
{
   while (len > 3)
   {
      *y++ = *x++;
      *y++ = *x++;
      *y++ = *x++;
      *y++ = *x++;
      len -= 4;
   }
   while (len)
   {
      *y++ = *x++;
      len--;
   }
}

#endif
