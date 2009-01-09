// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "Constellation.h"
#include "MersenneTwister.h"

#ifdef	FFTW3_FOUND
#include "fftw3.h"
#endif

using namespace std;

namespace FD {


class OFDModulator : public BufferedNode {
   
	int inputID;
	int outputID;

	int ncarriers;		// Total number of carriers ( = FFT size)
	int lprefix;		// Length of cyclic prefix
	Vector<float> gain;	// Gains vector

	int ndatacarriers;		// TODO: remove if not used
	int npilotcarriers;
	int nothercarriers;
	int ndatabits;

	Vector<Constellation> Xi;	// Set of available constellations

	enum CarrierType {DATA, PILOT, OTHER};
	vector<enum CarrierType> typeofcarrier;
	vector<int> Xiforcarrier;	// Index of constellation for each carrier 

#ifdef FFTW3_FOUND
	fftw_plan p;
	fftw_complex *fft_in, *fft_out;
#else
	Vector<complex<float> > fft_in;
	Vector<complex<float> > fft_out;
#endif

	MTRand rng;	// Used to simulate pilot symbols

	virtual enum CarrierType getCarrierType(int carrierindex){return DATA;}
	virtual int getConstellation(int carrierindex){return 0;}
	virtual int getPilotSymbol(int carrierindex)
	{
		return rng.randInt((1 << Xi[carrierindex].nbits()) - 1);
	}

public:
	OFDModulator(string nodeName, ParameterSet params);
	~OFDModulator();

	void calculate(int output_id, int count, Buffer &out);

};

}//namespace FD
