// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "OFDDemodulator.h"

using namespace std;

namespace FD {

DECLARE_NODE(OFDDemodulator)
/*Node
 *
 * @name OFDDemodulator
 * @category Communications:Receiver
 * @description OFDM demodulator.
 *
 * @input_name INPUT
 * @input_type Vector<complex<float> >
 * @input_description Input sequence of complex samples.
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description LLRs on OFDM information bits.
 *
 * @parameter_name NCARRIERS
 * @parameter_type int
 * @parameter_description Number of carriers.
 * @parameter_value 256
 *
 * @parameter_name LPREFIX
 * @parameter_type int
 * @parameter_description Length of cyclic prefix
 * @parameter_value 16
 *
 * @parameter_name FILENAME
 * @parameter_type string
 * @parameter_description Name of file containing the OFDDemodulator.
 * @parameter_value Xi.txt
END*/


OFDDemodulator::OFDDemodulator(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	int i;

	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

	if(parameters.exist("NCARRIERS"))
		ncarriers = dereference_cast<int>(parameters.get("NCARRIERS"));
	else ncarriers = 256;
	if((1 << int(log2(ncarriers))) != ncarriers)
		throw new NodeException(this,
				"OFDDemodulator: ncarriers is not a power of 2.", __FILE__, __LINE__);

	gain.resize(ncarriers);
	for(i = 0; i < ncarriers; i++) gain[i] = 1.;

#ifdef	FFTW3_FOUND
	fft_in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * ncarriers);
	fft_out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * ncarriers);
#else
	fft_in.resize(ncarriers);
	fft_out.resize(ncarriers);
#endif

	if(parameters.exist("LPREFIX"))
		lprefix = dereference_cast<int>(parameters.get("LPREFIX"));
	else lprefix = 16;

	if(lprefix < 0 || lprefix > ncarriers)
		throw new NodeException(this, "OFDDemodulator: \
				length of cyclic prefix is invalid.", __FILE__, __LINE__);


#ifdef	FFTW3_FOUND
	// FFTW initialization
	p = fftw_plan_dft_1d(ncarriers, fft_in, fft_out, FFTW_FORWARD, FFTW_ESTIMATE);
								// TODO: check if
								// FFTW_ESTIMATE is the
								// best solution.
#endif

	// cout << "TP0" << endl;
	// Xi.resize(4);
	// lc.resize(4);
	// cout << "TP1" << endl;

	ParameterSet ps;
	ps.add(string("TYPE"), ObjectRef(new String("QAM")));
	ps.add(string("ENERGY"), ObjectRef(new Float(1.)));
	ps.add(string("BIT_LLRS"), ObjectRef(new Bool(true)));
	ps.add(string("NBITS"), ObjectRef(new Int(2)));
	LLRComputer lc0(string("LC0"), ps);
	lc.resize(1, lc0);
	ps.add(string("NBITS"), ObjectRef(new Int(4)));
	LLRComputer lc1(string("LC1"), ps);
	lc.resize(2, lc1);
	ps.add(string("NBITS"), ObjectRef(new Int(6)));
	LLRComputer lc2(string("LC2"), ps);
	lc.resize(3, lc2);
	ps.add(string("NBITS"), ObjectRef(new Int(8)));
	LLRComputer lc3(string("LC3"), ps);
	lc.resize(4, lc3);

	// Determine the number of data carriers and pilot carriers
	ndatacarriers = npilotcarriers = nothercarriers = 0;
	ndatabits = 0;
	typeofcarrier.resize(ncarriers);
	Xiforcarrier.resize(ncarriers);
	for(i = 0; i < ncarriers; i++)
	{
		typeofcarrier[i] = getCarrierType(i);
		if(typeofcarrier[i] == DATA) ndatacarriers++;
		else if(typeofcarrier[i] == PILOT) npilotcarriers++;
		else nothercarriers++;

		Xiforcarrier[i] = getConstellation(i);
		ndatabits += lc[Xiforcarrier[i]].nbits();
	}

	// cout << "OFDDemodulator: end of constructor." << endl;

}


OFDDemodulator::~OFDDemodulator()
{
#ifdef FFTW3_FOUND
	fftw_destroy_plan(p);
       	fftw_free(fft_in);
	fftw_free(fft_out);
#else
	fft_in.destroy();
	fft_out.destroy();
#endif
}





void OFDDemodulator::calculate(int output_id, int count, Buffer &out)
{
	int i, j, k;

	ObjectRef inputRef = getInput(inputID, count);
	const Vector<complex<float> > &inputVec = object_cast<Vector<complex<float> > >(inputRef);
	int length = inputVec.size();
	if(length > ncarriers + lprefix)
		throw new NodeException(this, "OFDDemodulator: length of input vector > ncarriers + lprefix.",
				__FILE__, __LINE__);

	Vector<float> &output = *Vector<float>::alloc(ndatabits);
	out[count] = &output;


#ifdef	FFTW3_FOUND

	// Remove the cyclic prefix
	for(k = 0; k < ncarriers; k++)
	{
		fft_in[k][0] = inputVec[k + lprefix].real();
		fft_in[k][1] = inputVec[k + lprefix].imag();
	}


	// Compute the FFT
	fftw_execute(p);



	// Compute LLRs on OFDM input bits
	j = 0;	// Data bit index
	LLRComputer &lcomp = lc[Xiforcarrier[0]];
	complex<float> x;
	float sqrn = sqrt(ncarriers);
	for(i = 0; i < ncarriers; i++)
	{
		lcomp = lc[Xiforcarrier[i]];
		if(typeofcarrier[i] == DATA)
		{
			x = complex<float>(fft_out[(i + ncarriers / 2) % ncarriers][0],
					fft_out[(i + ncarriers / 2) % ncarriers][1]);
			x /= gain[i] * sqrn;
			 
			// cout << i << ": " << x << endl;

			for(k = 0; k < lcomp.nbits(); k++)
			{
				output[j++] = lcomp.calculate(x, k);
			}
		}
	}




#else	// FFTW3_FOUND

	// Multiply each subcarrier by gain
	/*
	for(i = 0; i < ncarriers; i++)
	{
		fft_in[i] = gain[i] * inputVec[i];
	} */


	// Compute the FFT
	// fftw_one(p, fft_in, fft_out);
	throw new NodeException(this, "OFDDemodulator: FFTW3 not available.", __FILE__, __LINE__);



	// Add the cyclic prefix
	/*
	for(k = 0; k < lprefix; k++)
		output[k] = fft_out[ncarriers - lprefix + k];
	for(k = 0; k < ncarriers; k++)
		output[k + lprefix] = fft_out[k];
	*/

#endif	// FFTW3_FOUND

}

}	//namespace FD
