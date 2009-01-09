// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"
#include "LLRComputer.h"

using namespace std;

namespace FD {

DECLARE_NODE(LLRComputer)
/*Node
 *
 * @name LLRComputer
 * @category Communications:Receiver
 * @description Compute LLRs w.r.t. the points of the given constellation.
 *
 * @input_name INPUT
 * @input_type Vector<complex<float> >
 * @input_description Input sequence.
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Generated sequence.
 *
 * @parameter_name TYPE
 * @parameter_type string
 * @parameter_description Modulation type (PAM, PSK, QAM, FILE)
 * @parameter_value PSK
 *
 * @parameter_name NBITS
 * @parameter_type int
 * @parameter_description Number of bits per symbol.
 * @parameter_value 1
 *
 * @parameter_name ENERGY
 * @parameter_type float
 * @parameter_description Average symbol energy per complex dimension.
 * @parameter_value 1.
 *
 * @parameter_name FILENAME
 * @parameter_type string
 * @parameter_description Name of file containing the constellation.
 * @parameter_value Xi.txt
 *
 * @parameter_name VARIANCE
 * @parameter_type float
 * @parameter_description Variance of Gaussian noise.
 * @parameter_value 1.
 *
 * @parameter_name BIT_LLRS
 * @parameter_type bool
 * @parameter_description If true, comupte bit LLRs; if false, compute symbol LLRs.
 * @parameter_value false; true
END*/


LLRComputer::LLRComputer(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params), Xi(params)
{
	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

	d_nbits = Xi.nbits();
	nsignals = 1 << d_nbits;

	if (parameters.exist("VARIANCE"))
		sigma = sqrt(dereference_cast<float> (parameters.get("VARIANCE")));
	else sigma = 1.;

	if (parameters.exist("BIT_LLRS"))
		bit_llrs = sqrt(dereference_cast<bool> (parameters.get("BIT_LLRS")));
	else bit_llrs = false;

	tllr.resize(nsignals);

}



void LLRComputer::calculate(int output_id, int count, Buffer &out)
{

	int i, j, k;
	float llr0, llr1;
	
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<complex<float> > &inputVec = object_cast<Vector<complex<float> > >(inputRef);
	int length = inputVec.size();

	Vector<float> &output = *Vector<float>::alloc(length * (bit_llrs ? d_nbits : nsignals));
	out[count] = &output;



	// Compute symbol LLRs
	float yr, yi, mag;
	float sigma22 = 2. * sigma * sigma;
	for (i = 0; i < length; i++)
	{
		yr = inputVec[i].real();
		yi = inputVec[i].imag();
		mag = yr * yr + yi * yi;
		for(j = 0; j < nsignals; j++)
		{
			tllr[j] = mag - (yr - Xi[j].real()) * (yr - Xi[j].real()) -
			       (yi - Xi[j].imag()) * (yi - Xi[j].imag());
			tllr[j] /= sigma22;
		}
	}



	if(bit_llrs)
	{
		// Compute bit LLRs
		for (i = 0; i < length; i++)
		{
			llr0 = -1.e9;
			llr1 = -1.e9;
			for(j = 0; j < d_nbits; j++)
			{
				for(k = 0; k < nsignals; k++)
				{
					if((k >> j) & 1) llr1 = MAXSTAR(llr1, tllr[k]);
					else llr0 = MAXSTAR(llr0, tllr[k]);
				}
				output[i * d_nbits + j] = llr1 - llr0;
			}
		}
	}
	else
	{
		// Copy symbol LLRs
		for (i = 0; i < length; i++)
		{
			for(j = 0; j < nsignals; j++)
			{
				output[i * nsignals + j] = tllr[j];
			}
		}
	}

}



float LLRComputer::calculate(complex<float> x, int b)
{

	int j, k;
	float llr0, llr1;
	

	// Compute symbol LLR
	float yr, yi, mag;
	float sigma22 = 2. * sigma * sigma;
	yr = x.real();
	yi = x.imag();
	mag = yr * yr + yi * yi;
	for(j = 0; j < nsignals; j++)
	{
		tllr[j] = mag - (yr - Xi[j].real()) * (yr - Xi[j].real()) -
		       (yi - Xi[j].imag()) * (yi - Xi[j].imag());
		tllr[j] /= sigma22;
	}



	// Compute bit LLR
	llr0 = -1.e9;
	llr1 = -1.e9;
	for(k = 0; k < nsignals; k++)
	{
		if((k >> b) & 1) llr1 = MAXSTAR(llr1, tllr[k]);
		else llr0 = MAXSTAR(llr0, tllr[k]);
	}
	return llr1 - llr0;

}
}	// namespace FD
