#include "stream_wrap.h"
#include <string>

int fileptr_streambuf::uflow()
{
   return fgetc(file);
}

int fileptr_streambuf::underflow()
{
   return fgetc(file);
}

int fileptr_streambuf::overflow(int c = EOF)
{
   fputc(c, file);
}

fileptr_streambuf::fileptr_streambuf(FILE *_file, bool _owner)
   : file(_file)
   , owner(_owner)
{

}


