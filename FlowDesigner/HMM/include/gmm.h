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



class GMM;

///The result of scoring a frame against a GMM
class Score {
public:
   ///The log score
   float score;

   ///The ID of the nearest gaussian in the GMM used
   int gaussian_id;

   ///Pointer to the frame scored
   Frame *frame;

   ///Pointer to the GMM used when scoring
   const GMM *gmm;

public:

   ///Friend of GMM class
   friend class GMM;
}
;

///Gaussian Mixture Model (GMM) class
class GMM {
public:

   typedef enum {real, accum} GMM_Mode;
protected:
   
   ///STL vector containing all the gaussians in the GMM
   vector<Gaussian *>  gaussians;

   ///STL vector containing all the apriori weights of the gaussians
   vector<float>       apriori;

   ///Number of gaussians in the GMM
   int                 nb_gaussians;

   ///Whether of not the GMM trained (like real/accum mode)
   GMM_Mode            mode;

   ///Number of frames aligned to (used to train) the GMM
   int                 nb_frames_aligned;

public:

   /**Construct a GMM with nb_gauss gaussians, dim dimensions and a
      covariance pseudo-factory*/
   GMM(int nb_gauss, int dim, Covariance *(*cov_new)(int)) 
      : gaussians(vector<Gaussian *>(nb_gauss,(Gaussian *)NULL))
      , apriori (vector<float>(nb_gauss,0.0)) 
      , nb_gaussians (nb_gauss)
      , mode(accum)
      , nb_frames_aligned(0)
   {
      for (int i=0;i<nb_gauss;i++)
         gaussians[i] = new Gaussian (dim, cov_new);
   }

   ///Returns the number of gaussians in the GMM
   int get_nb_gaussians() const {return nb_gaussians;}

   ///Returns the i'th gaussian
   Gaussian &gaussian (int i) const {return *(gaussians[i]);}

   ///Accumulates (adds) the frame the the i'th gaussian
   void accum_to_gaussian(int i, const Frame &fr)
   {
      gaussians[i]->accum_frame(fr);
      apriori[i]+=1.0;
      nb_frames_aligned++;
   }

   ///Randomly init the GMM with a list (STL vector) of frames
   void init(vector<Frame *> frames);

   ///Performs k-means training
   void kmeans1(vector<Frame *> frames, int nb_iterations = 1);

   void split1();

   ///Performs k-means training (using another GMM to score)
   void kmeans2(vector<Frame *> frames, GMM *gmm);

   ///Converts the GMM from accum mode to real mode
   void to_real();

   ///Converts the GMM from real mode to accum mode and set everything to zero
   void reset_to_accum_mode();

   ///Score a frame against the GMM
   Score score(Frame * fr) const;

   ///Double the number of gaussians
   void binary_split();

   ///Score a list (STL vector) of frames against the GMM
   vector<Score> score(vector <Frame *> fr) const;

}
;



#endif
