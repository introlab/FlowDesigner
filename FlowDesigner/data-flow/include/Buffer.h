// Copyright (C) 1999 Jean-Marc Valin

#ifndef BUFFER_H
#define BUFFER_H

#include "Object.h"
#include "ObjectRef.h"
#include "Exception.h"
#include <typeinfo>
//#include "Buffer.h"
#include <vector>
#include "misc.h"

/**A rotating buffer implementation.
   This buffer keeps the last N lines (frames) it has*/
class Buffer : public Object {
protected:
   /**Pointers to objects*/
   mutable vector<ObjectRef> data;

   mutable vector<int> flags;

   /**The number N of objects kept*/
   int bufferLength ;

   mutable int bufferPos;

   mutable int currentPos;


public:
   /**Constructor, requires the buffer length (bLength)*/
   Buffer(int bLength)
      : data(bLength)
      , flags(bLength,0)
      , bufferLength (bLength)
   {
      //for (int i=0;i<bLength;i++) 
      //   data[i]=Object::nilObject;

      bufferPos=-1;
      currentPos = -1;
   }

   /**Copy constructor (not implemented, do we need one?)*/
   Buffer(const Buffer&);

   /**Indexing operator, read-only*/
   inline ObjectRef &get(int ind) const;

   /**Indexing operator, also sets the indexed frame as being the current frame*/
   inline ObjectRef &operator[] (int ind);
   
   int isValid(int ind) const
   {
      int tmp = bufferPos+ind-currentPos;
      if (tmp < 0)
	 tmp += bufferLength;
      return flags[tmp];
   }

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
	 //data[bufferPos] = Object::nilObject;
	 flags[bufferPos] = 0;
      }
      currentPos = ind;
      flags[bufferPos] = 1;
      return data[bufferPos];
   }
   
   int tmp = bufferPos+ind-currentPos;
   if (tmp < 0)
      tmp += bufferLength;
   flags[tmp] = 1;
   return data[tmp];
}

inline ObjectRef &Buffer::get(int ind) const
{
   if (ind < 0 || ind <= currentPos-bufferLength || ind > currentPos)
   {
      throw new BufferException (this, "trying to access non-existing element",ind);
   }
   int tmp = bufferPos+ind-currentPos;
   if (tmp < 0)
      tmp += bufferLength;
   if (flags[tmp])
      return data[tmp];
   else 
      throw new BufferException (this, "trying to access non-existing element",ind);
}


inline void Buffer::printOn(ostream &out) const
{
   int i;
   //cerr << "printing... currentPos = " << currentPos << " bufferLength = " << bufferLength << endl;
   out << "<Buffer" << endl;
   for (i=max(0,currentPos-bufferLength+1);i<=currentPos;i++)
   {
      out << "< " << i << " ";
      if (isValid(i))
	 out << get(i);
      else
	 out << "nil";
   }
   out << " >" << endl;
}


inline Buffer::Buffer(const Buffer&)
{throw new BufferException(NULL,"use an ObjectRef instead",0);}

#endif
