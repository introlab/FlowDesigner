// Copyright (C) 2001 Jean-Marc Valin

#include <math.h>
#include <vector>
#include <stream.h>
#include "Object.h"

class FeatureMap;

ostream &operator << (ostream &out, const FeatureMap &cell);


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
   vector<float> mapData;
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
   
   FeatureMap (const FeatureMap &) {cerr << "don't call the FeatureMap copy constructor\n"; exit(1);}

   ~FeatureMap() 
   {
      if (!terminal) 
      {
         delete first; 
         delete second;
      }
   }

   void recursiveSplit (const vector<float *> &inData, const vector<float *> &outData, int level = 2);

   void split(const vector<float *> &inData, const vector<float *> &outData, int &bestDim, float &bestThreshold);

   void findThreshold(const vector<float *> &inData, const vector<float *> &outData, int dim, float &thresh, float &score);
   
   int setNumbering(int start=0);
   
   void calc(const float *in, float *out);

   //int belongs(float *vect) const;

   //void calcTemplate (const vector<float *> &features, vector<int> &templ) const;

   void printOn(ostream &out) const;

   void readFrom (istream &in);
   
   int getOutDimension() {return outDimension;}

   friend istream &operator >> (istream &in, FeatureMap &cell);
};

