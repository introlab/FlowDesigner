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
#ifndef GMM_H
#define GMM_H
#include "gaussian.h"
#include "covariance.h"

//typedef int(*tata)(int);

class GMM;

class Score {
public:
   float score;
   int gaussian_id;
   Frame *frame;
   GMM *gmm;
public:
   friend class GMM;
};

class GMM {
   vector<Gaussian *>  gaussians;
   vector<float>       apriori;
   int                 nb_gaussians;
   bool                trained;
   int                 nb_frames_aligned;
public:
   GMM(int nb_gauss, int dim, Covariance *(*cov_new)(int)) 
      : gaussians(vector<Gaussian *>(nb_gauss,(Gaussian *)NULL))
      , apriori (vector<float>(nb_gauss,0.0)) 
      , nb_gaussians (nb_gauss)
      , trained(false)
      , nb_frames_aligned(0)
   {
      for (int i=0;i<nb_gauss;i++)
         gaussians[i] = new Gaussian (dim, cov_new);
   }
   int get_nb_gaussians() const {return nb_gaussians;}
   Gaussian &gaussian (int i) const {return *(gaussians[i]);}
   void accum_to_gaussian(int i, const Frame &fr)
   {
      gaussians[i]->accum_frame(fr);
      apriori[i]+=1.0;
      nb_frames_aligned++;
   }
   void init(vector<Frame *> frames);
   void kmeans1(vector<Frame *> frames);
   void kmeans2(vector<Frame *> frames, GMM *gmm);
   void to_real();
   void reset_to_accum_mode();
   Score score(Frame * fr) const;
   void binary_split();
   vector<Score> score(vector <Frame *> fr) const;

};



#endif
