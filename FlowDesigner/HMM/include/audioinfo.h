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
#include "Object.h"
#include "Vector.h"

class Tag {
protected:
   int begin;
   int end;
   int phoneID;
   int stateID;
   int gaussianID;

   bool using_IDs;
   //Ptr<Gaussian> gaussian;
   //Ptr<State> state;
   //Ptr<Phone> phone;
public:
   Tag() 
      : begin(0)
      , end(0)
      , using_IDs(true)
   {}
};

/**Describes the content of an audio file*/
class AudioInfo : public Object {
protected:
   string ortho;
   int length;
   bool coarse_endpointed;
   int coarse_start;
   int coarse_end;
   bool fine_endpointed;
   int fine_start;
   int fine_end;
   //Vector<Tag> tags;

public:
   /**Full constructor*/
   AudioInfo(string _ortho, int _length) 
      : ortho(_ortho)
      , length(_length)
      , coarse_endpointed(false)
      , fine_endpointed(false)
   {
      
   }

   /**Default constructor*/
   AudioInfo() 
      : coarse_endpointed(false)
      , fine_endpointed(false) 
   {}

   /**Does the info file contain coarse endpoints?*/
   bool isCoarseEndpointed() const {return coarse_endpointed;}

   /**Does the info file contain fine endpoints?*/
   bool isFineEndpointed() const {return fine_endpointed;}

   /**Is the sample within coarse endpoints*/
   bool isWithinCoarse(int sample) {return sample > coarse_start && sample < coarse_end;}

   /**Is the sample within fine endpoints*/
   bool isWithinFine(int sample) {return sample > fine_start && sample < fine_end;}

   /** print function used for operator << */
   virtual void printOn(ostream &out=cout) const;

   /**Read function used for operator >> */
   void readFrom (istream &in=cin);

   //friend ostream &operator << (ostream &out, const GMM &gmm);
   friend istream &operator >> (istream &in, AudioInfo &info);

};

#endif
