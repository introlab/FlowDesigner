// Copyright (C) 1999 Jean-Marc Valin

#ifndef BUFFER_H
#define BUFFER_H

#include "Object.h"
#include "ObjectRef.h"
#include "Exception.h"
#include <typeinfo>
#include <vector>

/**A rotating buffer implementation.
   This buffer keeps the last N lines (frames) it has*/
class Buffer : public Object {
protected:
   /**Pointers to objects*/
   mutable std::vector<ObjectRef> data;

   mutable std::vector<int> flags;

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
      if (ind > currentPos || ind <= currentPos-bufferLength)
	 return false;
      int tmp = bufferPos+ind-currentPos;
      if (tmp < 0)
	 tmp += bufferLength;
      return flags[tmp];
   }

   /**Prints the Buffer*/
   void printOn(std::ostream &out = std::cout) const;

   int getCurrentPos() {return currentPos;}

};


class BufferException : public BaseException {
public:
   /**The constructor with the parameters*/
   BufferException(const Buffer *_buffer, std::string _message, int _element) 
      : buffer (_buffer)
      , message(_message)
      , element(_element)
   {}

   /**The print method*/
   virtual void print(std::ostream &out = std::cerr) 
   {
      out<< typeid(buffer).name() << " error: "<< message << ".\nElement " << element << std::endl;
      out << "Buffer is: \n";
      out << *buffer;
   }
protected:
   /**the buffer that generated the error*/
   const Buffer *buffer;

   /**the error message*/
   std::string message;

   /**The element for which the error occured*/
   int element;
};


inline ObjectRef & Buffer::operator[] (int ind) 
{
   if (ind < 0 || ind <= currentPos-bufferLength)
   {
      throw new BufferException (this, "trying to write to non-existing element",ind);
   }
   if (ind > currentPos)
   {
      int diff = ind-currentPos;
      while (diff--)
      {
	 bufferPos++;
	 if (bufferPos == bufferLength)
	    bufferPos=0;
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
      throw new BufferException (this, "trying to read non-existing element",ind);
   }
   int tmp = bufferPos+ind-currentPos;
   if (tmp < 0)
      tmp += bufferLength;
   if (flags[tmp])
      return data[tmp];
   else 
      throw new BufferException (this, "trying to read not initialized element",ind);
}


inline void Buffer::printOn(std::ostream &out) const
{
   int i;
   //cerr << "printing... currentPos = " << currentPos << " bufferLength = " << bufferLength << std::endl;
   out << "<Buffer" << std::endl;
   for (i=currentPos-bufferLength+1;i<=currentPos;i++)
   {
      if (i>=0)
      {
	 out << "< " << i << " ";
	 if (isValid(i))
	    out << get(i);
	 else
	    out << "nil";
      }
   }
   out << " >" << std::endl;
}


inline Buffer::Buffer(const Buffer&)
{throw new BufferException(NULL,"use an ObjectRef instead",0);}

#endif
