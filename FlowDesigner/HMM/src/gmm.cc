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
#include "gmm.h"

void GMM::init(vector<Frame *> frames)
{
   for (unsigned int i=0;i<frames.size();i++)
   {
      Frame &fr = *frames[i];
      int gaus = rand()%nb_gaussians;
#ifdef DEBUG
      cerr << "going to: " << gaus << endl;
#endif
      accum_to_gaussian(gaus,fr);
   }
}

void GMM::reset_to_accum_mode()
{
   nb_frames_aligned=0;
   for(int i=0;i<nb_gaussians;i++)
   {
      gaussians[i]->reset_to_accum_mode();
      apriori[i]=0;
   }
}

void GMM::kmeans2(vector<Frame *> frames, GMM *gmm)
{
   vector<Score> scores;
   scores = gmm->score(frames);
   reset_to_accum_mode();
   for (unsigned int i=0;i<frames.size();i++)
   {
#ifdef DEBUG
      cerr << "going to: " << scores[i].gaussian_id << " score: " << scores[i].score << endl;
#endif
      accum_to_gaussian(scores[i].gaussian_id,*(frames[i]));
   }
}
void GMM::kmeans1(vector<Frame *> frames)
{
   kmeans2(frames,this);
}

void GMM::binary_split()
{
   int old_size=nb_gaussians;
   nb_gaussians *= 2;
   gaussians.resize(nb_gaussians);
   apriori.resize(nb_gaussians);
   for (int i=0;i<old_size;i++)
   {
      gaussians[i+old_size]=new Gaussian(*(gaussians[i]));
      vector <float> &mean = gaussians[i+old_size]->getMean();
      for (unsigned int j=0;j<mean.size();j++)
         mean[j]+=.1;
   }
}

void GMM::to_real()
{
   for (int i=0;i<nb_gaussians;i++)
   {
      apriori[i]=log(apriori[i]/nb_frames_aligned);
      gaussians[i]->to_real();
   }
}


vector<Score> GMM::score(vector <Frame *> fr) const
{
   vector<Score> scores(fr.size());
   for (unsigned int i=0;i<fr.size();i++)
   {
      scores[i]=score(fr[i]);
   }
   return scores;
   
}

Score GMM::score(Frame * fr) const
{
   float min_dist = MAXFLOAT;
   int min_gauss = 0;
   Score frame_score;
   for (int j=0;j<nb_gaussians;j++)
   {
      float dist = gaussians[j]->mahalanobis(fr);
      if (dist < min_dist)
      {
         min_dist=dist;
         min_gauss = j;
      }
      //cerr << "mean " << j << ": " << dist << endl;
   }
   frame_score.score = min_dist;
   frame_score.gaussian_id = min_gauss;
   frame_score.gmm = this;
   frame_score.frame = fr;
   return frame_score;
}
