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

#ifndef BUFFER_H
#define BUFFER_H

#include "Object.h"
#include "ObjectRef.h"
#include "Exception.h"
#include <typeinfo>
//#include "Buffer.h"
#include <vector>

/**A rotating buffer implementation.
   This buffer keeps the last N lines (frames) it has*/
class Buffer : public Object {
protected:
   ///Pointer to raw float
   mutable vector<ObjectRef> data ;

   ///The number N of lines kept
   int bufferLength ;

   mutable int bufferPos;

   mutable int currentPos;


public:
   ///Constructor, requires the vector length (vLength) and the buffer length (bLength)
   Buffer(int bLength)
      : data(bLength)
      , bufferLength (bLength)
   {
      for (int i=0;i<bLength;i++) 
         data[i]=Object::nilObject;

      bufferPos=-1;
      currentPos = -1;
   }

   ///Copy constructor (not implemented, do we need one?)
   Buffer(const Buffer&);

   ///Indexing operator, also sets the indexed frame as being the current frame
   ObjectRef operator[] (int ind) const;
   
   ///Indexing operator, also sets the indexed frame as being the current frame
   ObjectRef & operator[] (int ind) ;
   
   ///Print
   void printOn(ostream &out = cout) const;

   int getCurrentPos() {return currentPos;}

};


class BufferException : public BaseException {
public:
   ///The constructor with the parameters
   BufferException(const Buffer *_buffer, string _message, int _element) 
      : buffer (_buffer)
      , message(_message)
      , element(_element)
   {}

   ///The print method
   virtual void print(ostream &out = cerr) 
   {
      out<< typeid(buffer).name() << " error: "<< message << ".\nElement " << element << endl;
      out << "Buffer is: \n";
      out << *buffer;
   }
protected:
   ///the buffer that generated the error
   const Buffer *buffer;

   ///the error message
   string message;

   //The element for which the error occured
   int element;
};


inline ObjectRef & Buffer::operator[] (int ind) 
{
   if (ind < 0 || ind <= currentPos-bufferLength)
   {
      throw new BufferException (this, "trying to access non-existing element",ind);
   }
   if (ind > currentPos)
   {
      int diff = ind-currentPos;
      while (diff--)
      {
	 bufferPos++;
	 if (bufferPos == bufferLength)
	    bufferPos=0;
	 data[bufferPos] = Object::nilObject;
      }
      currentPos = ind;
      return data[bufferPos];
   }
   
   int tmp = bufferPos+ind-currentPos;
   if (tmp < 0)
      tmp += bufferLength;
   return data[tmp];


   /*if (ind > currentPos)
   {
      for (int i=currentPos+1;i<=ind;i++)
         data[ind % bufferLength] = Object::nilObject;
      currentPos = ind;
   }
   if (ind < 0 || ind <= currentPos-bufferLength)
   {
      throw new BufferException (this, "trying to access non-existing element",ind);
   }
   return data[ind % bufferLength];
   */
}

inline ObjectRef Buffer::operator[] (int ind) const
{
      if (ind < 0 || ind <= currentPos-bufferLength)
   {
      throw new BufferException (this, "trying to access non-existing element",ind);
   }
   if (ind > currentPos)
   {
      int diff = ind-currentPos;
      while (diff--)
      {
	 bufferPos++;
	 if (bufferPos == bufferLength)
	    bufferPos=0;
	 data[bufferPos] = Object::nilObject;
      }
      currentPos = ind;
      return data[bufferPos];
   }
   
   int tmp = bufferPos+ind-currentPos;
   if (tmp < 0)
      tmp += bufferLength;
   return data[tmp];


   /*if (ind > currentPos)
   {
      for (int i=currentPos+1;i<=ind;i++)
         data[ind % bufferLength] = Object::nilObject;
      currentPos = ind;
   }
   if (ind < 0 || ind <= currentPos-bufferLength)
   {
      throw new BufferException (this, "trying to access non-existing element",ind);
   }
   return data[ind % bufferLength];
   */
}


inline void Buffer::printOn(ostream &out) const
{
   int i;
   //cerr << "printing... currentPos = " << currentPos << " bufferLength = " << bufferLength << endl;
   out << "<Buffer" << endl;
   for (i=max(0,currentPos-bufferLength+1);i<=currentPos;i++)
   {
      out << "< " << i << " < ";
      out << *((*this)[i]);
      out << " > > ";
      out << endl;
   }
   out << " > " << endl;
}


inline Buffer::Buffer(const Buffer&) 
{throw new BufferException(NULL,"use an ObjectRef instead",0);}

#endif
