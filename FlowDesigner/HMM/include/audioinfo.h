// Copyright (C) 1999 Jean-Marc Valin
#ifndef AUDIO_INFO_H
#define AUDIO_INFO_H

#include <string>
#include <iostream>
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
   //RCPtr<Gaussian> gaussian;
   //RCPtr<State> state;
   //RCPtr<Phone> phone;
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
   std::string ortho;
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
   AudioInfo(std::string _ortho, int _length) 
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
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   //friend std::ostream &operator << (std::ostream &out, const GMM &gmm);
   friend std::istream &operator >> (std::istream &in, AudioInfo &info);

};

#endif
