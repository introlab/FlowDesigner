#include "net_types.h"
#include <stdio.h>
#include <unistd.h>
#include <iostream>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

ObjectRef TrueObject(new Bool(true));
ObjectRef FalseObject(new Bool(false));

FILEPTR::FILEPTR(FILE *file) 
   : NetCType<FILE *> (file)
{}

FILEPTR::FILEPTR(const string &filename, const string &mode) 
   : NetCType<FILE *> (fopen(filename.c_str(), mode.c_str()))
{}

FILEPTR::~FILEPTR() 
{
   fclose(value);
}


FILEDES::FILEDES(int fd) 
   : NetCType<int> (fd)
{}

FILEDES::FILEDES(const string &filename, int mode) 
   : NetCType<int> (open(filename.c_str(), mode))
{}

FILEDES::~FILEDES() 
{
   close(value);
}
