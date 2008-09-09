/* Copyright (C) 2008 Mathieu Labbe

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
*/

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <iostream>
#include <list>
#include <fstream>
#include "Stream.h"


using namespace FD;

class WavInputSound;

DECLARE_NODE(WavInputSound)
/*Node
 *
 * @name WavInputSound
 * @category DSP:Audio
 * @description Open a stream on a wav file
 *
 * @output_name OUTPUT
 * @output_type InputStream
 * @output_description The input stream
 *
 * @parameter_name FILENAME
 * @parameter_type string
 * @parameter_description String for file name (*.wav)
 * @parameter_value
 *
END*/

class WavStreambuf : public std::streambuf {
   protected:
      virtual int overflow(int = EOF) {std::cerr << "Don't call wav_streambuf::overflow()" << std::endl;return 1;}
	virtual std::streamsize xsputn(const char *s, std::streamsize n){std::cerr << "Don't call wav_streambuf::xsputn()" << std::endl; return -1;}

      virtual int uflow()  {std::cerr << "Don't call wav_streambuf::uflow()" << std::endl;return 1;}
      virtual int underflow()  {std::cerr << "Don't call wav_streambuf::underflow()" << std::endl;return 1;}
      virtual std::streamsize xsgetn(char *s, std::streamsize n);
      virtual int pbackfail(int c) {std::cerr << "Don't call wav_streambuf::pbackfail()" << std::endl;return 1;}
   public:
      WavStreambuf(std::string fileName);
      virtual ~WavStreambuf();
   protected:
        bool readWavHeader();

      std::string _fileName;

      char* _data;
      long _dataLength;
      long _posData;
      
      bool _initialized;        
};

class WavIStream : public std::istream {
   WavStreambuf _streambuffer;
   public:
      WavIStream(std::string fileName) : std::istream(&_streambuffer), _streambuffer (fileName) {clear();}
};

WavStreambuf::WavStreambuf(std::string fileName) : _fileName(fileName), _data(0), _dataLength(0), _posData(0), _initialized(false)
{
   std::cerr << "Initialising WAV" << std::endl;
   if(!readWavHeader())
    {
        std::cerr << "WavStreambuf : Initialization failed !" << std::endl;
    }
    else {
        std::cerr << "WavStreambuf : Initialization succeeded !" << std::endl;
    }
}

WavStreambuf::~WavStreambuf()
{
    if(_data)
        delete _data;
}

std::streamsize WavStreambuf::xsgetn(char *s, std::streamsize n)
{
    if(_initialized) {
        std::cerr << "WavStreambuf : xsgetn ...n=" << n << " _posData=" << _posData << "/" << _dataLength <<  std::endl;
        if(n <= (_dataLength - _posData)) {
            memcpy(s, _data + _posData, n);
            _posData += n;
        }
        else if(n > _dataLength) {
            std::cerr << "WavStreambuf : Streamsize is bigger than the data... dataLength=" << _dataLength << " streamsize=" << n << std::endl;
            return 0;
        }
        else {
            std::cerr << "WavStreambuf : circle buffer..." << std::endl;
            int firstCpy = _dataLength - _posData;
            memcpy(s, _data + _posData, firstCpy);
            memcpy(s, _data, n - firstCpy);
            _posData = n - firstCpy;
        }
    }
    else {
        std::cerr << "WavStreambuf : WavInputSound isn't initialized...See previous error to know why." << std::endl;
    }
    
    return n;
}

bool WavStreambuf::readWavHeader()
{
    std::ifstream in;
    
    char buf4[5] = {0}; //Size 5 for holding 0 value at end for string comparison

    in.open(_fileName.c_str(), std::ios::in | std::ios::binary);
    if(!in.fail())
    {

        //--------------------------------
        //the header defined in details at http://www.sonicspot.com/guide/wavefiles.html
        //--------------------------------
        //       __________________________
        //      | RIFF WAVE Chunk          |
        //      |   groupID  = 'RIFF'      |
        //      |   riffType = 'WAVE'      |
        //      |    __________________    |
        //      |   | Format Chunk     |   |
        //      |   |   ckID = 'fmt '  |   |
        //      |   |__________________|   |
        //      |    __________________    |
        //      |   | Sound Data Chunk |   |
        //      |   |   ckID = 'data'  |   |
        //      |   |__________________|   |
        //      |__________________________|

        /*in.seekg (0, ios::end);
        length = is.tellg();
        in.seekg (0, ios::beg);*/

        long riffChunkSize;
        long fmtChunkSize;
        long dataChunkSize;

        short formatType;
        short numChannels;
        long sampleRate;
        long bytesPerSecond;
        short blockAlign;
        short bitsPerSample;
        short extraFormatBytesSize;

        // RIFF chunk
        // 4 bytes, Chunk ID "RIFF"
        // 4 bytes, Chunk data size (file size - 8)
        // 4 bytes, RIFF type "WAVE"
        in.read(buf4, 4); 
        if(strcmp(buf4, "RIFF") != 0) {
            std::cerr << "WavInputSound : failed read RIFF chunk :  " << buf4 << std::endl;
            in.close();
            return false;
        }
        in.read((char*)&riffChunkSize, 4); 
        in.read(buf4, 4); 
        if(strcmp(buf4, "WAVE") != 0) { 
            std::cerr << "WavInputSound : failed read WAVE type :  " << buf4 << std::endl;
            in.close();
            return false;
        }

        // FMT chunk
        // 4 bytes, Chunk ID "fmt "
        // 4 bytes, Chunk data size (16 + extraformatbyte)
        in.read(buf4, 4);
        if(strcmp(buf4, "fmt ") != 0) {
            std::cerr << "WavInputSound : failed read FMT chunk :  " << buf4 << std::endl;
            in.close();            
            return false;
        }
        in.read((char*)&fmtChunkSize, 4);
        in.read((char*)&formatType,2);
        in.read((char*)&numChannels,2);
        in.read((char*)&sampleRate, 4);
        in.read((char*)&bytesPerSecond,4);
        in.read((char*)&blockAlign,2);
        in.read((char*)&bitsPerSample,2);
        if(fmtChunkSize>16) {
            in.read((char*)&extraFormatBytesSize, 2);
            std::cerr << "WARNING : extra format bytes not yet handled... extraFormatBytesSize=" << extraFormatBytesSize << std::endl;
            in.seekg(extraFormatBytesSize, std::ios_base::cur);
        }

        // Print some info
        std::cerr << "fmtChunkSize = " << fmtChunkSize << std::endl;
        std::cerr << "formatType = " << formatType << std::endl;
        std::cerr << "numChannels = " << numChannels << std::endl;
        std::cerr << "sampleRate = " << sampleRate << std::endl;
        std::cerr << "bytesPerSecond = " << bytesPerSecond << std::endl;
        std::cerr << "bitsPerSample = " << bitsPerSample << std::endl;

        if(formatType != 1) {
            std::cerr << "WavInputSound : formatType is not supported... Only PCM is. " << std::endl;
            in.close();
            return false;
        }

        //Find data chunk
        long chunkSize;
        in.read(buf4, 4);
        while(strcmp(buf4, "data") != 0)
        {
            std::cerr << "WavInputSound : passing chunk : " << buf4 << std::endl;
            in.read((char*)&chunkSize, 4);
            in.seekg(chunkSize, std::ios_base::cur);
            in.read(buf4, 4);
        }

        // Data chunk
        // 4 bytes, Chunk ID "data"
        // 4 bytes, data
        //in.read(buf4, 4);
        if(strcmp(buf4, "data") != 0) {
            std::cerr << "WavInputSound : failed read data chunk :  " << buf4 << std::endl;
            in.close();
            return false;
        }
        in.read((char*)&dataChunkSize, 4);

        // Fill data buffer
        _dataLength = dataChunkSize;
        _data = new char[_dataLength];
        in.read(_data, _dataLength);

        // Print some info
        std::cerr << "Frame length (in bytes) = " << dataChunkSize << std::endl;
        std::cerr << "Frame length (in samples) = " << dataChunkSize / (bitsPerSample/8) << std::endl;
        
    }
    else {
        std::cerr << "WavInputSound : failed open file :  " << _fileName << std::endl;
        return false;
    }

    in.close();
    _initialized = true;
    return true;
}

/** A constant node contains a value that will never changes. */
class WavInputSound : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;
   
   /**The ID of the 'value' output*/
   int outputID;

public:

   /**Constructor, takes the name of the node and a set of parameters*/
   WavInputSound(std::string nodeName, ParameterSet params)
   : Node(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
   }
   
   void initialize()
   {
      Node::initialize();
      
      String fileName = object_cast <String> (parameters.get("FILENAME"));

        std::cerr << "WavInputSound : Creating stream " << fileName << std::endl;
         value = ObjectRef(new IStream(new WavIStream(fileName.c_str())));
         std::cerr << "done" << std::endl;
   }

   virtual ~WavInputSound()
   {
   }
   /**Ask for the node's output which ID (number) is output_id 
   and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw new NodeException (this, "Sound: Unknown output id", __FILE__, __LINE__);
   }
   
protected:
   /**Default constructor, should not be used*/
   WavInputSound() {throw new GeneralException("Sound copy constructor should not be called",__FILE__,__LINE__);}
   
};


