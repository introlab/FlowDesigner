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

int fd_streambuf::overflow(int c)
{
   unsigned char _c = c;
   //FIXME: How about EOF?
   write(fd, &_c, 1);
   return c;
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
      if (read(fd, &charBuf, 1) > 0)
         return charBuf;
      else
         return EOF;
   }
}



int fd_streambuf::underflow()
{
   if (takeFromBuf)
   {
      return charBuf;
   } else
   {
      if (read(fd, &charBuf, 1) <= 0)
      {
         return EOF;
      }
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
   int tot_read = 0;
   while (1) {
      int nbytes = read(fd, s+tot_read, n-tot_read);
      if (nbytes<=0)
      {
         if (tot_read)
            return tot_read;
         else
            return EOF;
      } else {
         tot_read += nbytes;
         if (tot_read == n)
            return tot_read;
      }
   }
   //Old implementation
   //cerr << "fd_streambuf::xsgetn read " << nbytes << " byte." << endl;
   //if (nbytes > 0)
   //   return nbytes;
   //else
   //{
   //   cerr << "read only " << nbytes << " bytes " << endl;
   //   return EOF;
   //}
}



pipe_streambuf::pipe_streambuf(const string &command, bool _waitOnClose)
   : ifd(-1)
   , ofd(-1)
   , pid(0)
   , waitOnClose(_waitOnClose)
   , takeFromBuf(false)
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
      //It's important to close the pipes we don't use, otherwise everything blocks at EOF
      close(ifiledes[1]);
      close(ofiledes[0]);
   } else if (pid==0)
   {
      //child
      //Close stdin and stdout... that's wierd, but that's how it must be done
      close(0);
      close(1);
      //FIXME: Must do error checking
      dup(ofiledes[0]);
      dup(ifiledes[1]);
      //Not sure here
      //close(ofiledes[0]);
      //close(ifiledes[1]);
      char *argv[4];
      argv[0] = "sh";
      argv[1] = "-c";
      argv[2] = const_cast<char *> (command.c_str());
      argv[3] = 0;
      //cerr << "exec\n";
      execv("/bin/sh", argv);
      throw new GeneralException("execv failed. Something really bad happened", __FILE__, __LINE__);
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

int pipe_streambuf::ll_read(void *buf, size_t count)
{
   int ori=count;
   int total=0;
   while (total<ori)
   {
      int res = 0;
      res = read(ifd, buf, count);
      if (res==0)
      {
	 //cerr << "got zero" << endl;
	 if (waitpid(pid, NULL, WNOHANG)==pid)
	 {
	    //cerr << "proc got killed?\n";
	    pid=0;
	 }
	 break;
      } else if (res==-1)
      {
	 //cerr << "ERROR" << endl;
	 perror("read");
	 break;
      }
      total+=res;
      count-=res;
   }
   return total;
}

int pipe_streambuf::ll_write(const void *buf, size_t count)
{
   int res = 0;
   res = write(ofd, buf, count);
   if (res==0)
   {
      if (waitpid(pid, NULL, WNOHANG)==pid)
      {
	 pid=0;
	 return 0;
      }
   }
   return res;
}


int pipe_streambuf::overflow(int c)
{
   unsigned char _c = c;
   //FIXME: How to handle EOF?
   if (ofd!=-1)
      return ll_write(&_c, 1);
   else
      throw new GeneralException("Cannot write to read-only pipe", __FILE__, __LINE__);
}


streamsize pipe_streambuf::xsputn(const char *s, streamsize n)
{
   if (ofd!=-1)
      return ll_write(s, n);
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
	 if (ll_read(&charBuf, 1))
	    return charBuf;
	 else
	    return EOF;
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
	 takeFromBuf = true;
	 if (ll_read(&charBuf, 1))
	    return charBuf;
	 else
	    return EOF;
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
      //int size = read(ifd, s, n);
      //return size;
      return ll_read(s,n);
   } else
      throw new GeneralException("Cannot read from write-only pipe", __FILE__, __LINE__);
}
#if 0
struct buff {
   int fd;
   char *data;
   int length;
};

void *send_thread(void *in)
{
   buff *b=(buff*)in;
   write(b->fd, b->data, b->length);
}
#endif

void pipe_streambuf::pipeString(const string &in, string &out)
{
#if 0
   pthread_t thr;
   buff b = {ifd, const_cast<char *>(in.c_str()), in.size()};
   pthread_create(&thr, NULL, send_thread, (void*)&b);
   int len=1024;
   char *buf;
   while (len==1024)
   {
      len=ll_read((void *)buf, 1024);
      cerr.write(buf,len);
   }
#endif
}


#endif

