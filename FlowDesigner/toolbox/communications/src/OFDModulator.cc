// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "OFDModulator.h"

using namespace std;

namespace FD {

DECLARE_NODE(OFDModulator)
/*Node
 *
 * @name OFDModulator
 * @category Communications:Transmitter
 * @description OFDM modulator.
 *
 * @input_name INPUT
 * @input_type Vector<int>
 * @input_description Input sequence of bits.
 *
 * @output_name OUTPUT
 * @output_type Vector<complex<float> >
 * @output_description The OFDM symbol.
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
 * @parameter_description Name of file containing the OFDModulator.
 * @parameter_value Xi.txt
END*/


OFDModulator::OFDModulator(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	int i;

	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

	if(parameters.exist("NCARRIERS"))
		ncarriers = dereference_cast<int>(parameters.get("NCARRIERS"));
	else ncarriers = 256;
	if((1  << int(log2(ncarriers))) != ncarriers)
		throw new NodeException(this,
				"OFDModulator: ncarriers is not power of 2.", __FILE__, __LINE__);

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
		throw new NodeException(this, "OFDModulator: \
				length of cyclic prefix is invalid.", __FILE__, __LINE__);

#ifdef	FFTW3_FOUND
	// FFTW initialization
	p = fftw_plan_dft_1d(ncarriers, fft_in, fft_out, FFTW_BACKWARD, FFTW_ESTIMATE);
								// TODO: check if
								// FFTW_ESTIMATE is the
								// best solution.
#endif

	Xi.resize(4);


	ParameterSet ps;
	ps.add(string("TYPE"), ObjectRef(new String("QAM")));
	ps.add(string("ENERGY"), ObjectRef(new Float(2.)));
	ps.add(string("NBITS"), ObjectRef(new Int(2)));
	Xi[0] = Constellation(ps);
	ps.add(string("ENERGY"), ObjectRef(new Float(4.)));
	ps.add(string("NBITS"), ObjectRef(new Int(4)));
	Xi[1] = Constellation(ps);
	ps.add(string("ENERGY"), ObjectRef(new Float(6.)));
	ps.add(string("NBITS"), ObjectRef(new Int(6)));
	Xi[2] = Constellation(ps);
	ps.add(string("ENERGY"), ObjectRef(new Float(8.)));
	ps.add(string("NBITS"), ObjectRef(new Int(8)));
	Xi[3] = Constellation(ps);



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

		ndatabits += Xi[Xiforcarrier[i]].nbits();
	}


}


OFDModulator::~OFDModulator()
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





void OFDModulator::calculate(int output_id, int count, Buffer &out)
{
	int i, j, k;

	ObjectRef inputRef = getInput(inputID, count);
	const Vector<int> &inputVec = object_cast<Vector<int> >(inputRef);
	int length = inputVec.size();
	if(length != ndatabits)
		throw new NodeException(this, "OFDModulator: length of input vector does not match the number \
				of data bits in one OFDM symbol.", __FILE__, __LINE__);

	Vector<complex<float> > &output = *Vector<complex<float> >::alloc(ncarriers + lprefix);

	out[count] = &output;

#ifdef	FFTW3_FOUND

	// Generate the IFFT input vector
	j = 0;	// Data carrier index
	complex<float> x;
	int s;
	for(i = 0; i < ncarriers; i++)
	{
		s = 0;
		if(typeofcarrier[i] == DATA)
		{
			for(k = 0; k < Xi[Xiforcarrier[i]].nbits(); k++)
			{
				s |= (inputVec[j++] & 1) << k;
			}
			x = Xi[Xiforcarrier[i]][s];
		}
		else if(typeofcarrier[i] == PILOT)
		{
			s = getPilotSymbol(i);
			x = Xi[Xiforcarrier[i]][s];
		}
		else x = complex<float>(0., 0.);

		fft_in[(i + ncarriers / 2) % ncarriers][0] = gain[i] * x.real(); 
		fft_in[(i + ncarriers / 2) % ncarriers][1] = gain[i] * x.imag();

	}



	// Compute the IFFT
	fftw_execute(p);



	// Add the cyclic prefix
	float sqrn = sqrt(ncarriers);
	for(k = 0; k < lprefix; k++)
		output[k] = complex<float>(fft_out[ncarriers - lprefix + k][0] / sqrn,
				fft_out[ncarriers - lprefix + k][1] / sqrn);
	for(k = 0; k < ncarriers; k++)
		output[k + lprefix] = complex<float>(fft_out[k][0] / sqrn, fft_out[k][1] / sqrn);

#else	// FFTW3_FOUND

	// Multiply each subcarrier by gain
	/*
	for(i = 0; i < ncarriers; i++)
	{
		fft_in[i] = gain[i] * inputVec[i];
	} */


	// Compute the FFT
	// fftw_one(p, fft_in, fft_out);
	throw new NodeException(this, "OFDModulator: FFTW3 not available.", __FILE__, __LINE__);



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
