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

#ifndef MATRIX_H
#define MATRIX_H

#include "Object.h"
//#include <vector>
#include "ObjectParser.h"


template<class T>
class Matrix : public Object
{
protected:
   int rows, cols;
   T *data;
public:
   Matrix() 
      : rows(0)
      , cols(0)
      , data(NULL)
   {}

   Matrix(int _rows, int _cols) 
      : rows(_rows)
      , cols(_cols)
      , data(new T [_rows*_cols])
   {}

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

   T *operator [] (int i)
   {
      return data+i*cols;
   }

   const T *operator [] (int i) const
   {
      return data+i*cols;
   }

   T &operator () (int i, int j)
   {
      return data[i*cols+j];
   }

   const T &operator () (int i, int j) const
   {
      return data[i*cols+j];
   }

   int nrows() const {return rows;} 

   int ncols() const {return cols;}

   void printOn(ostream &out) const
   {
      out << "<Matrix " << endl;
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
   
   virtual void rawWrite(ostream &out) const
   {
      //out.write ((const unsigned char*) begin(), size()*sizeof(T));
   }
   
   void readFrom(istream &in=cin);

};

template <class T>
inline void Matrix<T>::readFrom(istream &in)
{
   string tag;
   cerr << "reading matrix\n";
   int new_cols, new_rows;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("Parse error: '<' expected");
      in >> tag;
      if (tag == "rows")
         in >> new_rows;
      else if (tag == "cols")
         in >> new_cols;
      else if (tag == "data")
      {
	 cerr << "resizing\n";
	 resize(new_rows,new_cols);
	 cerr << "reading data...\n";
         for (int i=0;i<rows*cols;i++)
	    in >> data[i];
	 cerr << "done\n";
      } else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }

}


#endif
