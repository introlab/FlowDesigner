#include "stream_wrap.h"
#include <string>


fileptr_streambuf::fileptr_streambuf(FILE *_file, bool _owner)
   : file(_file)
   , owner(_owner)
   , takeFromBuf(false)
{

}

int fileptr_streambuf::overflow(int c = EOF)
{
   fputc(c, file);
   if (feof(file))
      return EOF;
   else
      return c;
}


streamsize fileptr_streambuf::xsputn(const char *s, streamsize n)
{
   return fwrite(s, 1, n, file);
}



int fileptr_streambuf::uflow()
{
   if (takeFromBuf)
   {
      takeFromBuf = false;
      return charBuf;
   } else {
      charBuf = fgetc(file);
      return charBuf;      
   }
}

int fileptr_streambuf::underflow()
{
   if (takeFromBuf)
   {
      return charBuf;
   } else
   {
      charBuf = fgetc(file);
      takeFromBuf = true;
      return charBuf;
   }
}

int fileptr_streambuf::pbackfail(int c)
{
   if (!takeFromBuf)
   {
      if (c != EOF)
	 charBuf = c;
      
      takeFromBuf = true;
      return charBuf;
   } else {
      return EOF;
   }
}

streamsize fileptr_streambuf::xsgetn(char *s, streamsize n)
{
   cerr << "xsgetn " << n << endl;
   return fread(s, 1, n, file);
}





#ifndef WIN32


fd_streambuf::fd_streambuf(int _fd, bool _owner)
   : fd(_fd)
   , owner(_owner)
   , takeFromBuf(false)
{
   
}

int fd_streambuf::overflow(int c = EOF)
{
   write(fd, &c, 1);
   //FIXME: determine EOF
   /*if (eof(file))
      return EOF;
   else
   return c;*/
}


streamsize fd_streambuf::xsputn(const char *s, streamsize n)
{
   return write(fd, s, n);
}



int fd_streambuf::uflow()
{
   if (takeFromBuf)
   {
      takeFromBuf = false;
      return charBuf;
   } else {
      read(fd, &charBuf, 1);
      return charBuf;      
   }
}

int fd_streambuf::underflow()
{
   if (takeFromBuf)
   {
      return charBuf;
   } else
   {
      read(fd, &charBuf, 1);
      takeFromBuf = true;
      return charBuf;
   }
}

int fd_streambuf::pbackfail(int c)
{
   if (!takeFromBuf)
   {
      if (c != EOF)
	 charBuf = c;
      
      takeFromBuf = true;
      return charBuf;
   } else {
      return EOF;
   }
}

streamsize fd_streambuf::xsgetn(char *s, streamsize n)
{
   return read(fd, s, n);
}

#endif

/*
int main() 
{
   fileptr_ostream out(stdout);
   out.write("salut\n", 6);
}

*/
