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
#ifndef _IMAGE_POOL_H_
#define _IMAGE_POOL_H_

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <map>
#include <list>
#include "misc.h"
#include "Image.h"

namespace RobotFlow {

class ImagePool {

 protected:
  
  size_t m_max_stored;
  std::map<int,std::list<Image*> >  m_image_map;
  
  public:

  ImagePool(int _max_stored=50) 
    : m_max_stored(_max_stored) {

  }
  
  Image* newImage (int width, int height, int pixelsize) {

    //cerr<<endl<<"Image allocation from pool :"<<width<<" "<<height<<" "<<pixelsize<<endl;    
    int size = width * height * pixelsize;

    std::map<int, std::list<Image*> >::iterator iter = m_image_map.find(size);

    if (iter != m_image_map.end()) {

      if ((*iter).second.empty()) {
	return new Image(width, height, pixelsize);
      }
      else {
	Image *_image = (*iter).second.front();
	(*iter).second.pop_front();
	_image->ref();
	
	//update width & height
	_image->update_size(width,height);
	
	return _image;
      }
    }
    else {
      //image of that size unavailable
      return new Image(width, height, pixelsize);
    }
  }

  void release(Image *image) {

    //cerr<<endl<<"Pool release image"<<endl;    
    int size = image->get_size();
         
    std::map<int, std::list<Image*> >::iterator iter = m_image_map.find(size);

    if (iter != m_image_map.end()) {
      if ((*iter).second.size() <= m_max_stored) {
	(*iter).second.push_back(image);
      }
      else {
	delete image;
      }
    }
    else {
      m_image_map.insert(std::make_pair(size,std::list<Image*>(1,image)));
    }
  }

  ~ImagePool() {

    for (std::map<int, std::list<Image*> >::iterator iter = m_image_map.begin();
	 iter != m_image_map.end(); iter++) {
    
      while (!(*iter).second.empty()) {
	Image *_image = (*iter).second.front();
	(*iter).second.pop_front();
	delete _image;
      }
    }
  }
};

}//namespace RobotFlow
#endif
