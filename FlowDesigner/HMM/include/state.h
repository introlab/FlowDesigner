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
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.#ifndef STATE_H
#ifndef STATE_H
#define STATE_H


#include "gmm.h"

///State class
class State {
   
   struct TransitionProbability {
      State *state;
      float probability;
   };

   ///Corresponding GMM
   GMM *mixture;
   
   ///All possible transitions to other states
   vector<TransitionProbability> transitions;
};


#endif
