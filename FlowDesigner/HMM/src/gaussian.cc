#include "gaussian.h"

#include "covariance.h"
#include <assert.h>
#include <iostream.h>
#include <fstream.h>

Gaussian::Gaussian()
{
  mean       = 0;
  covariance = 0;
  dimension  = 0;
}


Gaussian::Gaussian(int _dimension, float *_mean, float *_covariance, int _type)
{
  dimension = _dimension;
  setMean(_dimension, _mean, 1);
  setCovariance(_dimension, _covariance, _type, 1);
}


Gaussian::Gaussian(const Gaussian &_gaussian)
{
  dimension = _gaussian.getDimension();  
  setMean(dimension, _gaussian.getMean(), 1);
  if (covariance != 0)
    delete covariance;
  covariance = new Covariance(_gaussian.getCovariance());
}


Gaussian::~Gaussian()
{
  if (mean != 0)
  {
    delete mean;
    mean       = 0;
  }
  
  if (covariance != 0)
  {
    delete covariance;
    covariance = 0;
  }
}


void Gaussian::setMean(int _dimension, float *_mean, int _fromConstructor=0)
{
  assert(dimension == _dimension);
  
  if (!_fromConstructor && mean != 0)
  {
    delete mean;
    mean       = 0;
  }
  
  mean       = new float[dimension];
  for (int i=0; i<dimension; i++)
    mean[i] = _mean[i];
}


void Gaussian::setCovariance(int _dimension, float *_covariance, int _type, int _fromConstructor=0)
{
  assert(dimension == _dimension);

  if (!_fromConstructor && covariance != 0)
  {
    delete covariance;
    covariance = 0;
  }

  covariance = new Covariance(_dimension, _covariance, _type);
}


float Gaussian::probability(const Frame &f)
{
  return(0.5);
}


void Gaussian::print()
{
  if (dimension != 0)
  {
    cout << "==============================" << endl;
    cout << "Dimension: " << dimension << endl;

    cout << "Mean: " << endl;
    for (int i=0; i<dimension; i++)
      cout << mean[i] << ", ";
    cout << endl;
    
    cout << "Covariance: " << endl;
    covariance->print();
    cout << "==============================" << endl;
  }
}


void Gaussian::save(ofstream &file)
{
  file.write((char*) &dimension, sizeof(dimension));
  file.write((char*) mean,      sizeof(float)*dimension);
  covariance->save(file);
}


void Gaussian::load(ifstream &file)
{
  file.read((char*) &dimension, sizeof(dimension));
  if (mean != 0)
    delete mean;
  mean = new float[dimension];
  file.read((char*) mean, sizeof(float)*dimension);
  if (covariance != 0)
    delete covariance;
  covariance = new Covariance();
  covariance->load(file);
}
