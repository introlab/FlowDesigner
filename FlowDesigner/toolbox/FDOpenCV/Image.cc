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
#include "Image.h"
#include "ImagePool.h"
#include "BaseException.h"
#include "ObjectParser.h"
#include "binio.h"
#include <assert.h>
#include <sstream>
#include <fstream>

extern "C" {
  #include <jpeglib.h>
}

using namespace std;
using namespace FD;

namespace RobotFlow {

DECLARE_TYPE(Image);

static ImagePool _ImagePool(10);

Image::Image() 
  : m_width(0), m_height(0), m_size(0), m_data(NULL) {
  
}

Image::Image(int width, int height, int pixelsize) {

  m_width = width;
  m_height = height;
  m_size = width * height * pixelsize;
  m_data = new unsigned char[pixelsize * width * height];
}

Image::Image(const Image & cpy) {

  if (m_data) delete [] m_data;

  m_width = cpy.m_width;
  m_height = cpy.m_height;
  m_size = cpy.m_size;
  m_data = new unsigned char[m_size];
  memcpy(m_data,cpy.m_data,m_size);
}

Image::~Image() {

  cerr<<"image destructor"<<endl;

  if (m_data) delete [] m_data;

}

void Image::printOn(ostream &out) const {
  
  out << "<Image "<<endl;
  out << "<Width "<<m_width<<" >"<<endl;
  out << "<Height "<<m_height<<" >"<<endl;
  out << "<Size "<<m_size<<" >"<<endl;
  out << "<Data "<<endl;
  
  for (int i = 0; i < m_size; i++) {
    out<<(int) m_data[i]<<" ";
  }

  out <<" >"<<endl; //end of data
  out <<" >"<<endl; //end of Image
}

void Image::readFrom(istream &in) {

  string tag;
  
  while (1) {
    char ch;
    in >> ch;
    if (ch == '>') break;
    
    else if (ch != '<') {
      throw new ParsingException ("Image::readFrom : Parse error: '<' expected");
    }
    in >> tag;
    
    if (tag == "Width") {
      in >> m_width;
    }
    else if (tag == "Height") {
      in >> m_height;
    }
    else if (tag == "Size") {
      in >> m_size;
      //allocate image data
      m_data = new unsigned char[m_size];
    }
    else if (tag == "Data") {
      for (int i = 0; i < m_size; i++) {
	int data_in;
	in >> data_in; 
	m_data[i] = (unsigned char) data_in;
      }
    }
    else {
      throw new ParsingException ("Image::readFrom : Unknown argument: " + tag);
    }
    
    if (!in) throw new ParsingException ("Image::readFrom : Parse error trying to build " + tag);
    
    in >> tag;
    if (tag != ">") 
      throw new ParsingException ("Image::readFrom : Parse error: '>' expected ");
  }
}
  
void Image::serialize(ostream &out) const {

  out << "{" << "Image" << endl;
  out << "|";
  
  BinIO::write(out, &m_width, 1);
  BinIO::write(out, &m_height, 1);
  BinIO::write(out, &m_size, 1);

  for (int i = 0; i < m_size; i++) {
    BinIO::write(out, &m_data[i], 1);
  }

  out << "}";
}
  
void Image::unserialize(istream &in) {

  BinIO::read(in, &m_width, 1);
  BinIO::read(in, &m_height, 1);
  BinIO::read(in, &m_size, 1);
  

  m_data = new unsigned char[m_size];

  for (int i =0; i < m_size; i++) {
    BinIO::read(in, &m_data[i], 1);
    //cerr<<"reading : "<<(int)m_data[i]<<endl;
  }
  //last character
  
  char ch;
  in >> ch;
}

void Image::put_data(unsigned char* data, int size) {

  if (size == m_size) {
    memcpy(m_data,data,size);
  }
  else {
    cerr<<"Image::put_data : image size mismatch"<<endl;
  }

}

void Image::update_size(int width, int height) {

  m_width = width; 
  m_height = height;
}

Image* Image::alloc(int width, int height, int pixelsize) {
 // cerr<<"allocating image of "<<width<<" "<<height<<endl;
  return _ImagePool.newImage(width,height,pixelsize);
}

void Image::destroy() {
  _ImagePool.release(this);
}

Image *Image::sub_image(int x1, int y1, int x2, int y2) {

  int depth = m_size / (m_width * m_height);
  unsigned short *image_ptr = (unsigned short*) get_data();

  Image *new_image = NULL;
  unsigned short *new_image_ptr = NULL;

  switch(depth) {
  case 2:
    //creating new image
    new_image = Image::alloc((x2 - x1) + 1, (y2 - y1) + 1, 2);
    new_image_ptr = (unsigned short*) new_image->get_data();

    //copying image data from original to sub image region
    for (int i = 0; i < new_image->get_height(); i++) {
      for (int j = 0; j < new_image->get_width(); j++) {
	*new_image_ptr = image_ptr[((y1 + i) * m_width) + (x1 + j)];
	//next pixel
	new_image_ptr++;
      }
    }
    
    break;
  default:
    char message[256];
    sprintf(message,"Image Depth : %i not yet supported",depth);
    throw new GeneralException(message,__FILE__,__LINE__);
    break;   
  }

  return new_image;
}

/*
* Initialize destination --- called by jpeg_start_compress
* before any data is actually written.
*/
METHODDEF(void)
init_destination (j_compress_ptr cinfo)
{
  //cerr<<"init_destination"<<endl;
}

/*
* Empty the output buffer --- called whenever buffer fills up.
*/
METHODDEF(boolean)
empty_output_buffer (j_compress_ptr cinfo)
{
  //cerr<<"empty output buffer"<<endl;
  return TRUE;
}


/*
* Terminate destination --- called by jpeg_finish_compress
* after all data has been written.
*/
METHODDEF(void)
term_destination (j_compress_ptr cinfo)
{
  //cerr<<"term_destination"<<endl;
}

//DECOMPRESS HANDLERS

METHODDEF(void)
init_source (j_decompress_ptr cinfo)
{
  //cerr<<"init_source"<<endl;
}
  
METHODDEF(boolean)
fill_input_buffer(j_decompress_ptr cinfo)
{
  //cerr<<"fill_input_buffer"<<endl;
}

METHODDEF(void)
skip_input_data (j_decompress_ptr cinfo, long num_bytes)
{
  //cerr<<"skip_input_data"<<endl;
}

METHODDEF(boolean) 
resync_to_restart(j_decompress_ptr cinfo, int desired) 
{
  //cerr<<"resync_to_restart"<<endl;
}

METHODDEF(void) 
term_source (j_decompress_ptr cinfo)
{
  //cerr<<"term_source"<<endl;
}

METHODDEF(void)
output_message (j_common_ptr cinfo)
{
  //cerr<<"output_message"<<endl;
}


/* Conditionally emit a trace or warning message */
METHODDEF (void)
emit_message (j_common_ptr cinfo, int msg_level) {

  //cerr<<"emit_message level "<<msg_level<<endl;
  //TODO DO something with warning messages
}


int Image::jpeg_compress(unsigned char*  buffer, int max_size, int quality) {

    struct jpeg_compress_struct cinfo;
    struct jpeg_error_mgr jerr;
      
    cinfo.err = jpeg_std_error(&jerr);
  
    jpeg_create_compress(&cinfo);

    //set destination to memory
    struct jpeg_destination_mgr dest_mgr;

    //set function pointers
    dest_mgr.init_destination = init_destination;
    dest_mgr.empty_output_buffer = empty_output_buffer;
    dest_mgr.term_destination = term_destination;
    
    dest_mgr.next_output_byte = buffer;

    dest_mgr.free_in_buffer = max_size;	/* # of byte spaces remaining in buffer */
    cinfo.dest = &dest_mgr;
    
    //set other image params
    cinfo.image_width = get_width(); 	/* image width and height, in pixels */
    cinfo.image_height = get_height();

    switch(get_pixelsize()) {
    case 1:
      cinfo.input_components = 1;	/* # of color components per pixel */
       cinfo.in_color_space = JCS_GRAYSCALE;
       cinfo.data_precision = 8; //8 bpp
      break;

    case 2:
      cinfo.input_components = 3;	/* # of color components per pixel */
      cinfo.in_color_space = JCS_RGB; /* colorspace of input image */ 
      cinfo.data_precision = 15; //15 bPP
      break;

    case 3:
      cinfo.input_components = 3;	/* # of color components per pixel */
      cinfo.in_color_space = JCS_RGB; /* colorspace of input image */ 
      cinfo.data_precision = 24; //24 bPP
      break;
    }

    jpeg_set_defaults(&cinfo);

    jpeg_set_quality(&cinfo,quality,TRUE); /* set compression quality */

    /* Make optional parameter settings here */
    jpeg_start_compress(&cinfo, TRUE);

    JSAMPROW row_pointer[1];	/* pointer to a single row */
    int row_stride;		/* physical row width in buffer */  
    
    row_stride = get_width() * get_pixelsize(); /* JSAMPLEs per row in image_buffer */  

    unsigned char *image_buffer = get_data();
    
    while (cinfo.next_scanline < cinfo.image_height) {
      row_pointer[0] = & image_buffer[cinfo.next_scanline * row_stride];
      jpeg_write_scanlines(&cinfo, row_pointer, 1);
    }  

    jpeg_finish_compress(&cinfo); 

    int size = max_size - dest_mgr.free_in_buffer;

    jpeg_destroy_compress(&cinfo);

    //cerr<<"jpeg compress size : "<<size<<endl;

    //output number of free bytes
    return size;  
}

void Image::jpeg_decompress(const unsigned char* buffer, int size) {


  int image_width;
  int image_height;

  struct jpeg_decompress_struct cinfo;

  struct jpeg_error_mgr jerr;

  JSAMPROW row_pointer[1];	/* pointer to JSAMPLE row[s] */
  int row_stride;		/* physical row width in output buffer */
  int crows=0;

  jpeg_source_mgr source_mgr;

  //set handlers
  source_mgr.init_source = init_source;
  source_mgr.fill_input_buffer = fill_input_buffer;
  source_mgr.skip_input_data = skip_input_data;
  source_mgr.resync_to_restart = resync_to_restart;
  source_mgr.term_source = term_source;
  //set data pointers
  
  source_mgr.next_input_byte = buffer; /* => next byte to read from buffer */
  source_mgr.bytes_in_buffer = size;	/* # of bytes remaining in buffer */


  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  jpeg_std_error(&jerr);

  //change the output message handler
  //jerr.output_message = output_message;
  jerr.emit_message = emit_message;
  //jerr.trace_level = -10;
  cinfo.err = &jerr;
  

  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  /* Step 2: specify data source (memory) */
  cinfo.src = &source_mgr;


  /* Step 3: read file parameters with jpeg_read_header() */
  (void) jpeg_read_header(&cinfo, TRUE);

  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   * See libjpeg.doc for more info.
   */

  /* Step 4: set parameters for decompression */

  cinfo.out_color_space = JCS_RGB;

  /* In this example, we don't need to change any of the defaults set by
   * jpeg_read_header(), so we do nothing here.
   */

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* We may need to do some setup of our own at this point before reading
   * the data.  After jpeg_start_decompress() we have the correct scaled
   * output image dimensions available, as well as the output colormap
   * if we asked for color quantization.
   * In this example, we need to make an output work buffer of the right size.
   */ 

  /* JSAMPLEs per row in output buffer */
  row_stride = cinfo.output_width * cinfo.output_components;

  /* initialize new image */
  if (m_width != cinfo.image_width ||
      m_height != cinfo.image_height ||
      m_size != cinfo.output_components * cinfo.image_width * cinfo.image_height) {

    if (m_data) delete m_data;

    m_width = cinfo.image_width;
    m_height = cinfo.image_height;    
    m_data = new unsigned char[m_width * m_height * cinfo.output_components];
    m_size = m_width * m_height * cinfo.output_components;
  }


  /* Step 6: while (scan lines remain to be read) */
  /*           jpeg_read_scanlines(...); */

  /* Here we use the library's state variable cinfo.output_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   */
  while (cinfo.output_scanline < cinfo.output_height) {
    /* jpeg_read_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could ask for
     * more than one scanline at a time if that's more convenient.
     */
    row_pointer[0] = & m_data[crows * row_stride];
    (void) jpeg_read_scanlines(&cinfo, row_pointer, 1);
    
    crows++;
  }

  /* Step 7: Finish decompression */

  (void) jpeg_finish_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* Step 8: Release JPEG decompression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_decompress(&cinfo);


  /* At this point you may want to check to see whether any corrupt-data
   * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
   */

  /* And we're done! */
  //return retval;
}
 
Image::Image(const std::string &jpegfile) 
  : m_width(0), m_height(0), m_size(0), m_data(NULL) {

  load_jpeg(jpegfile);
}


void Image::load_jpeg(const std::string &jpegfile) {

  stringstream temp_stream;
  int count  = 0;
  ifstream input_file(jpegfile.c_str(),ios::binary);

  if (!input_file) {
    cerr<<"file not valid :"<<jpegfile<<endl;
  }
  else {      
    while(!input_file.eof()) {
      char value;
      input_file.read(&value,1);
      
      if (!input_file.eof()) {
	temp_stream.write(&value,1);    
      count++;
      }
    }     
    if (count > 0) {

      //deallocate image if necessary
      if (m_data) {
	m_width = 0;
	m_height = 0;
	delete [] m_data;
	m_data = NULL;
      }
    
      jpeg_decompress((const unsigned char*)temp_stream.str().c_str(),temp_stream.str().size());
    }
  }


  input_file.close();

}

void Image::save_jpeg(const std::string &jpegfile, int quality) {
  
  unsigned char buffer[m_width * m_height * 3];
  
  int size = jpeg_compress(buffer,m_width * m_height * 3,quality);

  ofstream outfile(jpegfile.c_str(),ios::binary);

  outfile.write((char *)buffer,size);

  outfile.close();
}


void Image::merge(Image &upperLeft, Image &upperRight, Image &lowerLeft, Image &lowerRight) {
    
  //could be removed once fully debugged
  assert(upperLeft.get_width() == m_width / 2);
  assert(upperLeft.get_height() == m_height / 2);
  assert(upperRight.get_width() == m_width / 2);
  assert(upperRight.get_height() == m_height / 2);
  assert(lowerLeft.get_width() == m_width / 2);
  assert(lowerLeft.get_height() == m_height / 2);
  assert(lowerRight.get_width() == m_width / 2);
  assert(lowerRight.get_height() == m_height / 2);

  //valid for all images
  unsigned char* merge_imagePtr = get_data();
  unsigned char* part_imagePtr = NULL;
  int merge_pixelsize = get_pixelsize();
  int merge_rowstride = merge_pixelsize * get_width();  
  int part_rowstride = merge_rowstride / 2;
  int half_width = get_width() / 2;
  int half_height = get_height() / 2;

  //upper left copy
  part_imagePtr = upperLeft.get_data();
  for(int i = 0; i < upperLeft.get_height(); i++) {
    memcpy(&merge_imagePtr[i * merge_rowstride],&part_imagePtr[i * part_rowstride],part_rowstride);   
  }

  //upper right copy
  part_imagePtr = upperRight.get_data();
  for(int i = 0; i < upperRight.get_height(); i++) {
    memcpy(&merge_imagePtr[i * merge_rowstride + (half_width * merge_pixelsize)],&part_imagePtr[i * part_rowstride],part_rowstride);   
  }

  //lower left
  part_imagePtr = lowerLeft.get_data();
  for(int i = 0; i < lowerLeft.get_height(); i++) {
    memcpy(&merge_imagePtr[(i + half_height) * merge_rowstride],&part_imagePtr[i * part_rowstride],part_rowstride);   
  }

  //lower right
  part_imagePtr = lowerRight.get_data();
  for(int i = 0; i < lowerRight.get_height(); i++) {
    memcpy(&merge_imagePtr[(i + half_height) * merge_rowstride + (half_width * merge_pixelsize)],&part_imagePtr[i * part_rowstride],part_rowstride);   
  }
}

void Image::rotate_180(Image &rotated_image) {

  if (rotated_image.get_size() != get_size()) {
    cerr<<"Images not of same size"<<endl;
  }

  unsigned char* rotated_ptr = rotated_image.get_data();
  unsigned char* self_ptr = &get_data()[get_size() -1];

  for (int i = 0; i < get_width() * get_height(); i++) {
    
    switch( get_pixelsize()) {
    case 3:
      rotated_ptr[0] = *(self_ptr - 2);
      rotated_ptr[1] = *(self_ptr -1);
      rotated_ptr[2] = *(self_ptr);
      break;
    default:
      cerr<<"Image::rotate_180 : unsupported size for rotation: "<<get_pixelsize()<<endl;
      break;
    }
    //increment pointer
    rotated_ptr += get_pixelsize();
    self_ptr -= get_pixelsize();
  }



}

}//namespace RobotFlow
