// Copyright (C) 1999 Jean-Marc Valin
// Copyright (C) 2004 Dominic Letourneau

#ifndef MATRIX_H
#define MATRIX_H

#include "Object.h"
#include "ObjectParser.h"
#include "typetraits.h"
#include "binio.h"

/**

Matrix class (template). Registered Matrices are : Matrix<bool>, Matrix<int>, Matrix<float>, 
Matrix<double>, Matrix<complex<float>>, Matrix<complex<double>>, Matrix<ObjectRef>.

\author Jean-Marc Valin (initial implementation)
\author Dominic Letourneau (added I/O capabilities)
\date 18/02/2004

*/
template<class T>
class Matrix : public Object
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

   static string GetClassName()
   {
      string name = ObjectGetClassName<Matrix<T> >();
      if (name == "unknown")
	 return string("Matrix");
      else
	 return name;
   }

};

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


/*
template <class T>
inline Matrix<T> operator * (Matrix<T> A, Matrix<T> B)
{
   if (A.nlines() != B.ncols())
      throw new GeneralException("Matrix<T>::operator *= A.nlines() != B.ncols()" __FILE__, __LINE__);
   
}
*/

/*The following code requires template partial specialization*/
#ifndef BROKEN_TEMPLATES

//FIXME: Serialize problems with (Object *)
template<class T, int I>
struct MatrixBinary {
   static inline void serialize(const Matrix<T> &m, ostream &out)
   {
      throw new GeneralException("MatrixBinary default serialize should never be called", __FILE__, __LINE__);
   }
   static inline void unserialize(Matrix<T> &m, istream &in)
   {
      throw new GeneralException("MatrixBinary default unserialize should never be called", __FILE__, __LINE__);
   }
};

template<class T>
struct MatrixBinary<T,TTraits::Object> {

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
};


template<class T>
struct MatrixBinary<T,TTraits::ObjectPointer> {
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
};


template<class T>
struct MatrixBinary<T,TTraits::Basic> {
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
};


template<class T>
struct MatrixBinary<T,TTraits::Unknown> {
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
};


template <class T>
inline void Matrix<T>::serialize(ostream &out) const
{
   MatrixBinary<T, TypeTraits<T>::kind>::serialize(*this, out);
}

template <class T>
inline void Matrix<T>::unserialize(istream &in)
{
   MatrixBinary<T, TypeTraits<T>::kind>::unserialize(*this, in);
}



#endif //BROKEN TEMPLATES

#endif
