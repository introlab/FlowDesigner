#ifndef _CELL_H_
#define _CELL_H_

// Copyright (C) 2001 Jean-Marc Valin

#include <math.h>
#include <vector>
#include <iostream>
#include "Object.h"

namespace FD {

class Cell;

inline double entropy_funct(double x) 
{
   if (x==0) {
      //std::cerr << "got zero\n"; 
      return 0;
   }
   return -x*log(x);
}

std::ostream &operator << (std::ostream &out, const Cell &cell);

class MutualInformation {
public:
   inline static float mmi (std::vector<int> Nij, int Nj, std::vector<int> Ai)
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
   
   Cell (const Cell &) {std::cerr << "don't call the Cell copy constructor\n"; exit(1);}

   ~Cell() 
   {
      if (!terminal) 
      {
         delete first; 
         delete second;
      }
   }

   void recursiveSplit (const std::vector<std::pair<int, float *> > &data, int level = 2);

   void split(const std::vector<std::pair<int, float *> > &data, int &bestDim, float &bestThreshold);

   void findThreshold(const std::vector<std::pair<int, float *> > &data, int dim, float &thresh, float &score);
   
   int setNumbering(int start=0);
   
   int belongs(float *vect) const;

   void calcTemplate (const std::vector<float *> &features, std::vector<int> &templ) const;

   void printOn(std::ostream &out) const;

   void readFrom (std::istream &in);

   friend std::istream &operator >> (std::istream &in, Cell &cell);
};


}//namespace FD

#endif
