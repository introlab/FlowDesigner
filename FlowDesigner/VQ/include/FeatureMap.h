#ifndef _FEATURE_MAP_H_
#define _FEATURE_MAP_H_

// Copyright (C) 2001 Jean-Marc Valin

#include <math.h>
#include <vector>
#include <iostream>
#include "Object.h"

namespace FD {

class FeatureMap;

std::ostream &operator << (std::ostream &out, const FeatureMap &cell);


class FeatureMap : public Object {
protected:
   int inDimension;
   int outDimension;
   bool terminal;
   FeatureMap *first;
   FeatureMap *second;
   float threshold;
   int splitDimension;
   int cellID;
   std::vector<float> mapData;
public:
   FeatureMap(int _inDimension, int _outDimension) 
      : inDimension(_inDimension)
      , outDimension(_outDimension)
      , terminal(true)
      , first(NULL)
      , second(NULL)
      , cellID (-1)
      , mapData(_outDimension)
   {}

   FeatureMap(){}
   
   FeatureMap (const FeatureMap &) {std::cerr << "don't call the FeatureMap copy constructor\n"; exit(1);}

   ~FeatureMap() 
   {
      if (!terminal) 
      {
         delete first; 
         delete second;
      }
   }

   void recursiveSplit (const std::vector<float *> &inData, const std::vector<float *> &outData, int level = 2);

   void split(const std::vector<float *> &inData, const std::vector<float *> &outData, int &bestDim, float &bestThreshold);

   void findThreshold(const std::vector<float *> &inData, const std::vector<float *> &outData, int dim, float &thresh, float &score);
   
   int setNumbering(int start=0);
   
   void calc(const float *in, float *out);

   //int belongs(float *vect) const;

   //void calcTemplate (const std::vector<float *> &features, std::vector<int> &templ) const;

   void printOn(std::ostream &out) const;

   void readFrom (std::istream &in);
   
   int getOutDimension() {return outDimension;}

   friend std::istream &operator >> (std::istream &in, FeatureMap &cell);
};

}//namespace FD

#endif
