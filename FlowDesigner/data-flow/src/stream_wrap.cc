#include "stream_wrap.h"
#include <string>
#include "BaseException.h"
#include "Object.h"

//@implements core

DECLARE_TYPE(EOFObject)


fileptr_streambuf::fileptr_streambuf(FILE *_file, bool _owner, bool _isPipe)
   : file(_file)
   , owner(_owner)
   , takeFromBuf(false)
   , isPipe(_isPipe)
{

}

int fileptr_streambuf::overflow(int c)
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
   return fread(s, 1, n, file);
}





#ifndef WIN32

#include <sys/wait.h>
#include <unistd.h>

fd_streambuf::fd_streambuf(int _fd, bool _owner)
   : fd(_fd)
   , owner(_owner)
   , takeFromBuf(false)
{
   
}

int fd_streambuf::overflow(int c = EOF)
{
   write(fd, &c, 1);
   //FIXME: Find EOF?
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





/*pipe_streambuf::pipe_streambuf(int _ifd, int _ofd, pid_t _pid, bool _waitOnClose)
   : ifd(_ifd)
   , ofd (_ofd)
   , pid(_pid)
   , waitOnClose(_waitOnClose)
   , takeFromBuf(false)
{
}
*/



pipe_streambuf::pipe_streambuf(const string &command, bool _waitOnClose)
   : ifd(-1)
   , ofd(-1)
   , pid(0)
   , waitOnClose(_waitOnClose)
{
   int ifiledes[2];
   int ofiledes[2];
   if (pipe(ifiledes) || pipe(ofiledes))
      throw new GeneralException("pipe_streambuf: cannot oen pipe, out of some resource?", __FILE__, __LINE__);
   pid = fork();
   if (pid>0)
   {
      //parent
      ifd=ifiledes[0];
      ofd=ofiledes[1];
      while(0)
      {
	 char tata[1000];
	 int N = read(ifd, tata, 1);
	 cerr.write(tata,N);
	 cerr.flush();
      }
   } else if (pid==0)
   {
      //child
      //Close stdin and stdout... that's wierd, but that's how it must be done
      close(0);
      close(1);
      //FIXME: Must do error checking
      cerr << "dup1: " << dup(ofiledes[0]) << endl;
      cerr << "dup2: " << dup(ifiledes[1]) << endl;
      char *argv[4];
      argv[0] = "sh";
      argv[1] = "-c";
      //argv[2] = "ls /";
      argv[2] = const_cast<char *> (command.c_str());
      argv[3] = 0;
      cerr << "exec\n";
      execv("/bin/sh", argv);
      cerr << "You should never, ever see this!" << endl;
   } else 
      throw new GeneralException("pipe_streambuf: cannot fork process, out of some resource?", __FILE__, __LINE__);
}

pipe_streambuf::~pipe_streambuf()
{
   if (ifd != -1)
      close(ifd);
   if (ofd != -1)
      close(ofd);
   if (pid)
   {
      if (waitOnClose)
	 waitpid(pid, NULL, 0);
      else
	 waitpid(pid, NULL, WNOHANG);
   }
}


int pipe_streambuf::overflow(int c = EOF)
{
   if (ofd!=-1)
      write(ofd, &c, 1);
   else
      throw new GeneralException("Cannot write to read-only pipe", __FILE__, __LINE__);
   //FIXME: Find EOF?
}


streamsize pipe_streambuf::xsputn(const char *s, streamsize n)
{
   if (ofd!=-1)
      return write(ofd, s, n);
   else
      throw new GeneralException("Cannot write to read-only pipe", __FILE__, __LINE__);
}



int pipe_streambuf::uflow()
{
   if (ifd!=-1)
   {
      if (takeFromBuf)
      {
	 takeFromBuf = false;
	 return charBuf;
      } else {
	 read(ifd, &charBuf, 1);
	 return charBuf;
      }
   } else
      throw new GeneralException("Cannot read from write-only pipe", __FILE__, __LINE__);
}

int pipe_streambuf::underflow()
{
   if (ifd!=-1)
   {
      if (takeFromBuf)
      {
	 return charBuf;
      } else
      {
	 read(ifd, &charBuf, 1);
	 takeFromBuf = true;
	 return charBuf;
      }
   } else
      throw new GeneralException("Cannot read from write-only pipe", __FILE__, __LINE__);
}

int pipe_streambuf::pbackfail(int c)
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

streamsize pipe_streambuf::xsgetn(char *s, streamsize n)
{
   if (ifd!=-1)
   {
      int size = read(ifd, s, n);
      return size;
   } else
      throw new GeneralException("Cannot read from write-only pipe", __FILE__, __LINE__);
}



#endif

