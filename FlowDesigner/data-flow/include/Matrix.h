// Copyright (C) 1999 Jean-Marc Valin
// Copyright (C) 2004 Dominic Letourneau

#ifndef MATRIX_H
#define MATRIX_H

#include "Object.h"
#include "ObjectParser.h"
#include "typetraits.h"
#include "binio.h"
#include "net_types.h"

class BaseMatrix : public Object {

public:   
   ///constructor
   BaseMatrix(){}

   ///return matrix size 
   virtual size_t msize() const = 0;
   
   ///return true if matrix empty
   virtual bool mempty() const = 0;
      
   /**
   	Returns an element (in an ObjectRef) of the matrix
	\param _row the row of the element
	\param _col the col of the element
	\return ObjectRef the newly created Object
   */
   virtual ObjectRef getIndex(int _row, int _col) {
   	throw new GeneralException(string("Matrix index not implemented for object : ") + className(),__FILE__,__LINE__);
   }
	
   /**
   	Sets an element (in an ObjectRef) of the matrix
	\param _row the row of the element
	\param _col the col of the element
	\param val the new value of the element
   */
   virtual void setIndex(int _row, int _col, ObjectRef val) {
   	throw new GeneralException(string("Matrix index not implemented for object : ") + className(),__FILE__,__LINE__);
   }

   /**
      Clone the matrix and return an identical copy (deep copy)
   */
   virtual ObjectRef clone() = 0;

};

/**

Matrix class (template). Registered Matrices are : Matrix<bool>, Matrix<int>, Matrix<float>, 
Matrix<double>, Matrix<complex<float>>, Matrix<complex<double>>, Matrix<ObjectRef>.

\author Jean-Marc Valin (initial implementation)
\author Dominic Letourneau (added I/O capabilities)
\date 18/02/2004

*/
template<class T>
class Matrix : public BaseMatrix
{
protected:

  ///number of rows in the Matrix
  int rows;
  
  ///number of cols in the Matrix
  int cols;
    
  //data pointer
  T *data;

public:

   ///You can always get the type of the Matrix by using typename Matrix<T>::basicType.
   typedef T basicType;

   ///Default constructor, will initialize Matrix dimensions to (0,0).
   Matrix() 
      : rows(0)
      , cols(0)
      , data(NULL)
   {}

   /**
      Copy constructor
      \param mat The matrix to copy
      \param transpose transpose matrix if true, defaults to false
   */
   Matrix(const Matrix &mat, bool transpose=false) 
      : rows(mat.rows)
      , cols(mat.cols)
      , data(new T [rows*cols])
   {
      if (transpose)
      {
	 rows=mat.cols;
	 cols=mat.rows;
	 for (int i=0;i<rows;i++)
	    for (int j=0;j<cols;j++)
	       data[i*cols+j] = mat.data[j*mat.cols+i];
      } else {
	 for (int i=0;i<rows*cols;i++)
	    data[i] = mat.data[i];
      }
   }

   /**
      Constructor with _rows and _cols
      \param _rows number of rows in the matrix
      \param _cols number of cols in the matrix
   */
   Matrix(int _rows, int _cols) 
      : rows(_rows)
      , cols(_cols)
      , data(new T [_rows*_cols])
   {}
   
   ///Destructor delete matrix data
   virtual ~Matrix() {delete [] data;}


   /**
      Resize a matrix (keeps elements already inserted into the Matrix). 
      Elements out of range in the new matrix are discarded.
      \param _rows new number of rows of the matrix
      \param _cols new number of cols of the matrix
   */
   void resize(int _rows, int _cols)
   {
      //cerr << "resizing to " << _rows << " x " << _cols << endl;
      T *new_data = new T [_rows*_cols];
      int min_rows = _rows < rows ? _rows : rows;
      int min_cols = _cols < cols ? _cols : cols;
      //cerr << min_rows << " " << min_cols << endl;
      for (int i=0;i<min_rows;i++)
	 for (int j=0;j<min_cols;j++)
	    new_data[i*_cols+j] = data[i*cols+j];
      //cerr << "deleting\n";
      if (data)
	 delete [] data;
      data = new_data;
      cols = _cols;
      rows = _rows;
   }

   /**
      operator[] returns the data pointer at a determined row.
      \param i the row number
      \return T* The data pointer to the first element of the row i   
   */
   T *operator [] (int i)
   {
      return data+i*cols;
   }

   /**
      operator[] returns the data pointer at a determined row.
      \param i the row number
      \return T* The data pointer to the first element of the row i   
   */
   const T *operator [] (int i) const
   {
      return data+i*cols;
   }


   /**
      operator(i,j) returns the element at row i, col j.
      \param i the row number
      \param j the col number
      \return T The element at position (i,j)
   */
   T &operator () (int i, int j)
   {
      return data[i*cols+j];
   }

   /**
      operator(i,j) returns the element at row i, col j.
      \param i the row number
      \param j the col number
      \return T The element at position (i,j)
   */
   const T &operator () (int i, int j) const
   {
      return data[i*cols+j];
   }

   ///returns the number of rows
   int nrows() const {return rows;} 

   ///returns the number of cols
   int ncols() const {return cols;}
   
   

   ///transpose the matrix (i,j) becomes (j,i), cols = rows, rows = cols.
   void transpose()
   {
      if (rows==cols)
      {
	 for (int i=0;i<rows;i++)
	    for (int j=0;j<i+1;j++)
	    {
	       float tmp=data[i*cols+j];
	       data[i*cols+j] = data[j*cols+i];
	       data[j*cols+i] = tmp;
	    }
      } else {
	 Matrix mat(*this);
	 for (int i=0;i<rows;i++)
	    for (int j=0;j<cols;j++)
	       data[i*cols+j] = mat.data[j*mat.cols+i];
      }
   }
   
   /**
      Formatted Matrix output in the FlowDesigner Format.<br>
      <b>Format :</b> \<Matrix\<T\> \<rows <i>nrows</i> \> \<cols <i>ncols</i> \> \<data <i> element(0,0) element(0,1)  ... element(rows-1,cols-1)</i>\> \>
      \param out the output stream
   */
   void printOn(ostream &out) const
   {
      out << "<"<<className() << endl;
      out << "<rows " << rows << ">" << endl;
      out << "<cols " << cols << ">" << endl;
      out << "<data " << endl;
      for (int i=0;i<rows;i++)
      {
	 for (int j=0;j<cols;j++)
	    out << data[i*cols + j] << " ";
	 out << endl;
      }
      out << ">" << endl;
      out << ">\n";
   }
   
   /*
   virtual void rawWrite(ostream &out) const
   {
       //out.write ((const unsigned char*) begin(), size()*sizeof(T));
   }
   */
   

   /**
      Formatted Matrix input in the FlowDesigner Format.<br>
      <b>Format :</b> \<Matrix\<T\> \<rows <i>nrows</i> \> \<cols <i>ncols</i> \> \<data <i> element(0,0) element(0,1)  ... element(rows-1,cols-1)</i>\> \>
      \param in the input stream
   */
   void readFrom(istream &in=cin);

   //(DL) 11/02/2004
   /** 
       Returns the size of the matrix 
       \return int size (cols * rows)
   */
   int size() const {
     return (cols * rows);
   }

   ///return matrix size 
   virtual size_t msize() const {return cols * rows;}
   
   ///return true if matrix empty
   virtual bool mempty() const {return (cols == 0 && rows == 0);}
   

   /**
      Binary Matrix output in the FlowDesigner Format.<br>
      <b>Format :</b> {Matrix\<T\> |<i>rows;cols;element(0,0);element(0,1)  ... element(rows-1,cols-1)</i>}
      \param out the output stream
   */
   virtual void serialize(ostream &out) const;


   /**
      Binary Matrix input in the FlowDesigner Format.<br>
      <b>Format :</b> {Matrix\<T\> |<i>rows;cols;element(0,0);element(0,1)  ... element(rows-1,cols-1)</i>}
      \param in the input stream
   */
   virtual void unserialize(istream &in);

   ///returns the class name
   static string GetClassName()
   {
      string name = ObjectGetClassName<Matrix<T> >();
      if (name == "unknown")
	 return string("Matrix");
      else
	 return name;
   }
   
   /**
   	Returns an element (in an ObjectRef) of the matrix
	\param _row the row of the element
	\param _col the col of the element
	\return ObjectRef the newly created Object
   */
   virtual ObjectRef getIndex(int _row, int _col);
   
   
   /**
   	Sets an element (in an ObjectRef) of the matrix
	\param _row the row of the element
	\param _col the col of the element
	\param val the new value of the element
   */
   virtual void setIndex(int _row, int _col, ObjectRef val);


   /**
      Clone the matrix and return an identical copy (deep copy) 
   */
   virtual ObjectRef clone();

};

///clone default implementation
template <class T>
inline ObjectRef Matrix<T>::clone() {
  
  Matrix<T> *cpy = new Matrix<T>(this->nrows(), this->ncols());
  
  for (int i = 0; i < this->nrows(); i++) {
    for (int j = 0; j < this->ncols(); j++) {
      (*cpy)(i,j) = (*this)(i,j);
    }
  }
  return ObjectRef(cpy);
}


///clone implementation with Matrix<ObjectRef>
template <>
inline ObjectRef Matrix<ObjectRef>::clone() {
  
  Matrix<ObjectRef> *cpy = new Matrix<ObjectRef>(this->nrows(), this->ncols());
  
  for (int i = 0; i < this->nrows(); i++) {
    for (int j = 0; j < this->ncols(); j++) {
      //cloning every Object in the matrix
      (*cpy)(i,j) = (*this)(i,j)->clone();
    }
  }

  return ObjectRef(cpy);
}

template <class T>
inline void Matrix<T>::readFrom(istream &in)
{
   string tag;
   //cerr << "reading matrix\n";
   int new_cols, new_rows;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("Matrix<T>::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "rows")
         in >> new_rows;
      else if (tag == "cols")
         in >> new_cols;
      else if (tag == "data")
      {
	 //cerr << "resizing\n";
	 resize(new_rows,new_cols);
	 //cerr << "reading data...\n";
         for (int i=0;i<rows*cols;i++)
	    in >> data[i];
	 //cerr << "done\n";
      } else
         throw new ParsingException ("Matrix<T>::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("Matrix<T>::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Matrix<T>::readFrom : Parse error: '>' expected ");
   }

}

//FIXME: Serialize problems with (Object *)
template<class T, int I>
struct MatrixMethod {
   static inline void serialize(const Matrix<T> &m, ostream &out)
   {
      throw new GeneralException("MatrixMethod default serialize should never be called", __FILE__, __LINE__);
   }
   static inline void unserialize(Matrix<T> &m, istream &in)
   {
      throw new GeneralException("MatrixMethod default unserialize should never be called", __FILE__, __LINE__);
   }
   static inline ObjectRef getIndex(Matrix<T> &m, int _row, int _col) 
   {
     throw new GeneralException("MatrixMethod getIndex should never be called", __FILE__, __LINE__);   				
   }   
   static inline void setIndex(Matrix<T> &m, int _row, int _col, ObjectRef val) 
   {
     throw new GeneralException("MatrixMethod setIndex should never be called", __FILE__, __LINE__);   				
   }
};

template<class T>
struct MatrixMethod<T,TTraits::Object> {

  static inline void serialize(const Matrix<T> &m, ostream &out) {
    out << "{" << m.className() << endl;
    out << "|";

    //writing nb rows
    int tmp = m.nrows();
    BinIO::write(out, &tmp, 1);
   
    //writing nb cols
    tmp = m.ncols();
    BinIO::write(out, &tmp, 1);
    
    //serializing object(s)
    for (size_t i=0;i<m.nrows();i++) {
      for (size_t j=0;j < m.ncols(); j++) {
	m(i,j).serialize(out);
      }
    }
    out << "}";
  }
  
  static inline void unserialize(Matrix<T> &m, istream &in)
   {
     int ncols, nrows;
     string expected = Matrix<T>::GetClassName();
     
     //reading matrix dimensions
     BinIO::read(in, &nrows, 1);
     BinIO::read(in, &ncols, 1);

     //resize matrix
     m.resize(nrows,ncols);

     //read every object
     for (size_t i=0;i<m.nrows();i++) {
       for (size_t j=0;j<m.ncols();j++) {
	 if (!isValidType(in, expected))
	   throw new ParsingException("Expected type " + expected);
	 m(i,j).unserialize(in);
       }
     }

     //reading ending "}"
     char ch;
     in >> ch;
   }

   static inline ObjectRef getIndex(Matrix<T> &m, int _row, int _col) 
   {
     if (_row < 0 || _row >= m.nrows() ||
	 _col < 0 || _col >= m.ncols() ) {
       throw new GeneralException("Matrix getIndex : index out of bound",__FILE__,__LINE__);
     }

     return ObjectRef(m(_row,_col).clone());
   }   
   static inline void setIndex(Matrix<T> &m, int _row, int _col, ObjectRef val) 
   {

     if (_row < 0 || _row >= m.nrows() ||
	 _col < 0 || _col >= m.ncols() ) {
       throw new GeneralException("Matrix setIndex : index out of bound",__FILE__,__LINE__);
     }
     RCPtr<T> obj = val;
     m(_row,_col) = *obj;
   }

};


template<class T>
struct MatrixMethod<T,TTraits::ObjectPointer> {
   static inline void serialize(const Matrix<T> &m, ostream &out)
   {
      out << "{" << m.className() << endl;
      out << "|";
     
      //writing nb rows
      int tmp = m.nrows();
      BinIO::write(out, &tmp, 1);
      
      //writing nb cols
      tmp = m.ncols();
      BinIO::write(out, &tmp, 1);

      //serializing object(s)
      for (size_t i=0;i<m.nrows();i++) {
	for (size_t j=0;j < m.ncols(); j++) {
	  m(i,j)->serialize(out);
	}
      }

      out << "}";
   }

   static inline void unserialize(Matrix<T> &m, istream &in)
   {
     int nrows,ncols;
     
     //reading matrix dimensions
     BinIO::read(in, &nrows, 1);
     BinIO::read(in, &ncols, 1);

     //resize matrix
     m.resize(nrows,ncols);

     for (size_t i=0;i<m.nrows();i++) {
       for (size_t j=0;j<m.ncols();j++) {
	 in >> m(i,j);
       }
     }

     char ch;
     in >> ch;
   }

   static inline ObjectRef getIndex(Matrix<T> &m, int _row, int _col)        
   {
     if (_row < 0 || _row >= m.nrows() ||
	 _col < 0 || _col >= m.ncols() ) {
       throw new GeneralException("Matrix getIndex : index out of bound",__FILE__,__LINE__);
     }
     return m(_row,_col);

   }   
   static inline void setIndex(Matrix<T> &m, int _row, int _col, ObjectRef val) 
   {
     
     if (_row < 0 || _row >= m.nrows() ||
	 _col < 0 || _col >= m.ncols() ) {
       throw new GeneralException("Matrix setIndex : index out of bound",__FILE__,__LINE__);
     }
     m(_row,_col) = val;
   }

};


template<class T>
struct MatrixMethod<T,TTraits::Basic> {
   static inline void serialize(const Matrix<T> &m, ostream &out)
   {
      out << "{" << m.className() << endl;
      out << "|";

      //writing nb rows
      int tmp = m.nrows();
      BinIO::write(out, &tmp, 1);
      
      //writing nb cols
      tmp = m.ncols();
      BinIO::write(out, &tmp, 1);

      //writing all data at once
      BinIO::write(out,m[0], m.size());

      out << "}";
   }
   static inline void unserialize(Matrix<T> &m, istream &in)
   {
     int nrows,ncols;

     //reading matrix dimensions
     BinIO::read(in, &nrows, 1);
     BinIO::read(in, &ncols, 1);
     
     //resize matrix
     m.resize(nrows,ncols);

     //reading all data at once
     BinIO::read(in,m[0], m.size());
     char ch;
     in >> ch;
   }
   static inline ObjectRef getIndex(Matrix<T> &m, int _row, int _col) 
   {
     if (_row < 0 || _row >= m.nrows() ||
	 _col < 0 || _col >= m.ncols() ) {
       throw new GeneralException("Matrix getIndex : index out of bound",__FILE__,__LINE__);
     }

     return ObjectRef(NetCType<T>::alloc(m(_row,_col)));
   }   
   static inline void setIndex(Matrix<T> &m, int _row, int _col, ObjectRef val) 
   {

     if (_row < 0 || _row >= m.nrows() ||
	 _col < 0 || _col >= m.ncols() ) {
       throw new GeneralException("Matrix setIndex : index out of bound",__FILE__,__LINE__);
     }

     RCPtr<NetCType<T> > obj = val;
     m(_row,_col) = *obj;
   }

};


template<class T>
struct MatrixMethod<T,TTraits::Unknown> {
  static inline void serialize(const Matrix<T> &m, ostream &out)
   {
      throw new GeneralException(string("Sorry, can't serialize this kind of object (") + typeid(T).name()
				 + ")", __FILE__, __LINE__);
   }
   static inline void unserialize(Matrix<T> &m, istream &in)
   {
      throw new GeneralException(string("Sorry, can't unserialize this kind of object (") + typeid(T).name()
				 + ")", __FILE__, __LINE__);
   }
   static inline ObjectRef getIndex(Matrix<T> &m, int _row, int _col) 
   {
     throw new GeneralException(string("Sorry, can't getIndex this kind of object (") + typeid(T).name()
			       + ")", __FILE__, __LINE__);
   }   
   static inline void setIndex(Matrix<T> &m, int _row, int _col, ObjectRef val) 
   {
     throw new GeneralException(string("Sorry, can't setIndex this kind of object (") + typeid(T).name()
				+ ")", __FILE__, __LINE__);
   }
};


template <class T>
inline void Matrix<T>::serialize(ostream &out) const
{
   MatrixMethod<T, TypeTraits<T>::kind>::serialize(*this, out);
}

template <class T>
inline void Matrix<T>::unserialize(istream &in)
{
   MatrixMethod<T, TypeTraits<T>::kind>::unserialize(*this, in);
}

template <class T>
inline ObjectRef Matrix<T>::getIndex(int _row, int _col)
{
  return MatrixMethod<T, TypeTraits<T>::kind>::getIndex(*this,_row,_col);
}

template <class T>
inline void Matrix<T>::setIndex(int _row, int _col, ObjectRef val)
{
  MatrixMethod<T,TypeTraits<T>::kind>::setIndex(*this,_row,_col,val);
}


#endif
