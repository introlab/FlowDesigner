// Copyright (C) 2001 Jean-Marc Valin

#include <math.h>
#include <vector>
#include <iostream>
#include "Object.h"

class Cell;

inline double entropy_funct(double x) 
{
   if (x==0) {
      //cerr << "got zero\n"; 
      return 0;
   }
   return -x*log(x);
}

ostream &operator << (ostream &out, const Cell &cell);

class MutualInformation {
public:
   inline static float mmi (vector<int> Nij, int Nj, vector<int> Ai)
   {
      float ent = 0;
      for (int i = 0; i < Nij.size(); i++)
      {
         float Pc = float(Nij[i]) / Nj;
         float Pa = float(Ai[i])  / Nij[i];
         ent -=  Pc * log(Pc)  +  Pc * ( Pa * log(Pa) + (1-Pa) * log (1-Pa) );
      }
      return ent;
   }
};

class Cell : public Object {
protected:
   int dimension;
   int numberClasses;
   bool terminal;
   Cell *first;
   Cell *second;
   float threshold;
   int splitDimension;
   int cellID;
public:
   Cell(int _dimension, int _numberClasses) 
      : dimension(_dimension)
      , numberClasses (_numberClasses)
      , terminal(true)
      , first(NULL)
      , second(NULL)
      , cellID (-1)
   {}

   Cell(){}
   
   Cell (const Cell &) {cerr << "don't call the Cell copy constructor\n"; exit(1);}

   ~Cell() 
   {
      if (!terminal) 
      {
         delete first; 
         delete second;
      }
   }

   void recursiveSplit (const vector<pair<int, float *> > &data, int level = 2);

   void split(const vector<pair<int, float *> > &data, int &bestDim, float &bestThreshold);

   void findThreshold(const vector<pair<int, float *> > &data, int dim, float &thresh, float &score);
   
   int setNumbering(int start=0);
   
   int belongs(float *vect) const;

   void calcTemplate (const vector<float *> &features, vector<int> &templ) const;

   void printOn(ostream &out) const;

   void readFrom (istream &in);

   friend istream &operator >> (istream &in, Cell &cell);
};

