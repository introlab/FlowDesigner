// ***** A Gaussian estimator

class Covariance;
class Frame;
class ifstream;
class ofstream;

class Gaussian 
{
private:
  int         dimension;
  float      *mean;
  Covariance *covariance;
  
public:
  Gaussian();
  Gaussian(int, float *, float*, int);
  Gaussian(const Gaussian &);

  ~Gaussian();
  
  void setMean(int, float *, int);
  void setCovariance(int, float *, int, int);

  float probability(const Frame &);

  int         getDimension() const  { return dimension; }
  float      *getMean()      const  { return mean; }
  Covariance &getCovariance() const { return *covariance; }

  void load(ifstream &);
  void save(ofstream &);

  void print();
};

  
      
