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
#ifndef AUDIO_INFO_H
#define AUDIO_INFO_H

#include <string>
#include <stream.h>
#include <iostream.h>

class Tag {
   string tagName;
   
};

/**Describes the content of an audio file*/
class AudioInfo {
protected:
   string ortho;
   int length;
   bool coarse_endpointed;
   int coarse_start;
   int coarse_end;
   bool fine_endpointed;
   int fine_start;
   int fine_end;
   //vector<Tag> tags;

public:
   AudioInfo(string _ortho, int _length) 
      : ortho(_ortho)
      , length(_length)
      , coarse_endpointed(false)
      , fine_endpointed(false)
   {
      
   }

   /** print function used for operator << */
   virtual void printOn(ostream &out=cout) const;

   /**Read function used for operator >> */
   void readFrom (istream &in=cin);

   //friend ostream &operator << (ostream &out, const GMM &gmm);
   friend istream &operator >> (istream &in, AudioInfo &info);

};

#endif
