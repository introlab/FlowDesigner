/* Copyright (C) 2002 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#ifndef _IMAGE_H_
#define _IMAGE_H_

#include "Object.h"

namespace RobotFlow {

class Image : public FD::Object {

  friend class ImagePool;

 public:

  Image();

  Image(int width, int height, int pixelsize);

  Image(const Image & cpy);

  Image (const std::string &jpegfile);

  virtual ~Image();
  
  virtual void printOn(std::ostream &out = std::cout) const;
  
  virtual void readFrom(std::istream &in=std::cin);
  
  virtual void serialize(std::ostream &out) const;
  
  virtual void unserialize(std::istream &in);

  int get_size() {return m_size;}

  int get_width() {return m_width;}

  int get_height() {return m_height;}

  int get_pixelsize() {return m_size / (m_width * m_height);}

  unsigned char* get_data() {return m_data;}

  void put_data(unsigned char* data, int size);

  Image* sub_image(int x1, int y1, int x2, int y2);
  
  static Image* alloc(int width, int height, int pixelsize);

  virtual void destroy();

  int jpeg_compress(unsigned char*  buffer, int max_size, int quality);

  void jpeg_decompress(const unsigned char* buffer, int size);

  void load_jpeg(const std::string &jpegfile);

  void save_jpeg(const std::string &jpegfile, int quality);

  void rotate_180(Image &rotated_image);
  
  void merge(Image &upperLeft, Image &upperRight, Image &lowerLeft, Image &lowerRight);

 private:

  void update_size(int width, int height); 

  unsigned char* m_data;
  int m_width;
  int m_height;
  int m_size;

};

}//namespace RobotFlow

#endif
