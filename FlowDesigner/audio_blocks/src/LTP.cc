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

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class LTP;

DECLARE_NODE(LTP)
/*Node
 *
 * @name LTP
 * @category Signal:DSP
 * @description Long-term predictor, finds best correlation (pitch) within (START <= sample delay <= END)
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input frame
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description [pitch gain, pitch period]
 *
 * @parameter_name START
 * @parameter_type int
 * @parameter_description Smallest pitch allowed
 *
 * @parameter_name END
 * @parameter_type int
 * @parameter_description Largest pitch allowed
 *
END*/


class LTP : public BufferedNode {
   
   int inputID;
   int outputID;
   int start;
   int end;

public:
   LTP(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      start = dereference_cast<int> (parameters.get("START"));
      end = dereference_cast<int> (parameters.get("END"));
      inputsCache[inputID].lookBack=1;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(2);
      out[count] = &output;


      const Vector<float> *past;
      bool can_look_back = false;
      if (count > 0)   
      {
         ObjectRef pastInputValue = getInput(inputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }      

      float best=-FLT_MAX;
      float best_gain=0;
      int best_T=0;
      float scores[end+1];
      float gains[end+1];
      /*static float old_scores[2000];
      static bool init=false;
      if (!init)
      {
	 for (int i=0;i<2000;i++)
	    old_scores[i]=0;
	 init=true;
	 }*/

      for (int i=0;i<start;i++)
	 scores[i]=0;
      for (int lag=start;lag<=end;lag++)
      {
	 float corr=0;
	 float energy=0;
	 for (int i=lag;i<inputLength;i++)
	 {
	    corr += in[i]*in[i-lag];
	    energy += in[i-lag]*in[i-lag];
	 }
	 if (can_look_back)
	    for (int i=0;i<lag;i++)
	    {
	       corr += in[i] * (*past)[inputLength+i-lag];
	       energy += (*past)[inputLength+i-lag] * (*past)[inputLength+i-lag];
	    } 
	 float gain = corr/(energy+.000000001);
	 float score = corr*corr/(energy+.000000001);
	 scores[lag]=score;
	 gains[lag]=gain;
	 //cout << corr/energy << " ";
	 if (score > best)
	 {
	    //prevents period doubling
	    if (1 || score/best > 1.6 || abs(lag-2*best_T) > 5)
	    {
	       best = score;
	       best_T = lag;
	       best_gain = corr/(energy+.00000001);
	    } //else {cerr << "doubling\n";}
	 }
      }
      //cout << endl;
/*
      for (int i=start;i<=end;i++)
      {
	 scores[i] += .6*old_scores[i];
	 old_scores[i]=scores[i];
	 cout << scores[i] << " ";
      }
      cout << endl;
*/
      for (int i=4;i>=2;i--)
      {
	 int div = best_T/i;
	 float max_score = 0;
	 float max_gain = 0;
	 int max_T=0;
	 for (int j=div-2; j <= div+2 ; j++)
	 {
	    if (scores[j]>max_score)
	    {
	       max_score = scores[j];
	       max_gain = gains[j];
	       max_T = j;
	    }
	 }
	 if (max_score*2 > best || max_gain > .3)
	 {
	    if (i==2)
	    {
	       int div2 = best_T+max_T;
	       float max_score2 = 0;
	       float max_gain2 = 0;
	       int max_T2=0;
	       for (int j=div2-2; j <= div2+2 && j<end; j++)
	       {
		  if (scores[j]>max_score2)
		  {
		     max_score2 = scores[j];
		     max_gain2 = gains[j];
		     max_T2 = j;
		  }		  
	       }
	       if (!(max_score2*2 > best || max_gain > .2))
		  continue;
	    }
	    best_T = max_T;
	    best_gain = max_gain;
	    break;
	 }
      }
      
      //cout << endl;
      if (best_gain > 1.2)
	 best_gain = 1.2;
      if (best_gain < -.2)
	 best_gain = -.2;
      //cout << endl;
      output[0] = best_gain;
      output[1] = best_T;
      //cout << output[0] << " " << output[1] << endl;
   }

};
