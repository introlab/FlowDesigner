#ifndef _STREAM_H_
#define _STREAM_H_

#include "Object.h"
#include <iostream>
#include <string>
#include <fstream>
#include <stdio.h>
#include "ObjectPool.h"
#include "ObjectParser.h"
#include "typetraits.h"

namespace FD {

//(DL 19/02/2004)
//Moved stream code here instead of net_types.h
//

/**Base Overflow Stream type, wraps a C++ stream
   @author Jean-Marc Valin
*/
class Stream : public Object
{
  protected:
   int owner;
  public:
   Stream(bool _owner=true)
      : owner(_owner)
      {}
   virtual ~Stream() {}
};

/**Overflow IStream (input Stream) type, wraps a C++ istream
   @author Jean-Marc Valin
*/
class IStream : virtual public Stream {
  
  std::istream *int_istream;
  public:
   IStream(std::istream *_str, bool _owner=true)
      : Stream(_owner)
      , int_istream(_str)
      {}
   IStream &read (char *ch, int len) {int_istream->read(ch,len); return *this;}
   int gcount() {return int_istream->gcount();}
   int eof() {return int_istream->eof();}
   int fail() {return int_istream->fail();}
   IStream &getline (char *ch, int len) {int_istream->getline(ch,len); return *this;}
   operator std::istream &() {return *int_istream;}
   void printOn(std::ostream &out) const {out << "<IStream unknown>";}
   ~IStream() {if (owner) {delete dynamic_cast<std::istream *>(int_istream);owner=false;}}

   IStream &seekg (int pos, std::ios::seekdir dir) {int_istream->seekg(pos, dir); return *this;}

   template <class T>
   IStream &operator >> (T &obj) {*int_istream >> obj; return *this;}

};

/**Overflow OStream (output Stream) type, wraps a C++ ostream
   @author Jean-Marc Valin
*/
class OStream : virtual public Stream {
  std::ostream *int_ostream;
  public:
   OStream(std::ostream *_str, bool _owner=true)
      : Stream(_owner)
      , int_ostream(_str)
      {}
   OStream &write (const char *ch, int len) {int_ostream->write(ch,len); return *this;}
   int eof() {return int_ostream->eof();}
   int fail() {return int_ostream->fail();}
   void flush() {int_ostream->flush();}
   operator std::ostream &() {return *int_ostream;}
   void printOn(std::ostream &out) const {out << "<OStream unknown>";}
   ~OStream() {if (owner) {delete dynamic_cast<std::ostream *>(int_ostream);owner=false;}}

   OStream &seekp (int pos, std::ios::seekdir dir) {int_ostream->seekp(pos, dir); return *this;}

   template <class T>
   OStream &operator << (const T &obj) {*int_ostream << obj; return *this;}

};

/**Overflow IOStream (input/output Stream) type, wraps a C++ iostream
   @author Jean-Marc Valin
*/
class IOStream : public IStream, public OStream {
  std::iostream *int_iostream;
  public:
   IOStream(std::iostream *_str, bool _owner=true)
      : Stream(_owner)
      , IStream(_str, _owner)
      , OStream(_str, _owner)
      , int_iostream(_str)
      {}
   int eof() {return int_iostream->eof();}
   int fail() {return int_iostream->fail();}
   operator std::iostream &() {return *int_iostream;}
   void printOn(std::ostream &out) const {out << "<IOStream unknown>";}
   ~IOStream() {if (owner) {delete dynamic_cast<std::iostream *>(int_iostream);owner=false;}}

   IOStream &seekg (int pos, std::ios::seekdir dir) {int_iostream->seekg(pos, dir); return *this;}
   IOStream &seekp (int pos, std::ios::seekdir dir) {int_iostream->seekp(pos, dir); return *this;}

};


}//end namespace FD




#endif
