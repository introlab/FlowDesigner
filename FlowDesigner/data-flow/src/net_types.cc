// Copyright (C) 2001 Jean-Marc Valin

#include "net_types.h"
#include <stdio.h>

#ifndef WIN32
#include <unistd.h>
#endif /*ifdef WIN32*/

#include <iostream>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

//@implements core

DECLARE_TYPE2(Int, 0)
DECLARE_TYPE2(Bool, 1)
DECLARE_TYPE2(Float, 2)
DECLARE_TYPE2(Double, 3)
DECLARE_TYPE2(String, 4)

vector<Int *> ObjectPool<Int>::stack;
vector<Bool *> ObjectPool<Bool>::stack;
vector<Float *> ObjectPool<Float>::stack;
vector<Double *> ObjectPool<Double>::stack;

ObjectRef TrueObject(new Bool(true));
ObjectRef FalseObject(new Bool(false));

FILEPTR::FILEPTR(FILE *file) 
   : GenericType<FILE *> (file)
{}

FILEPTR::FILEPTR(const string &filename, const string &mode) 
   : GenericType<FILE *> (fopen(filename.c_str(), mode.c_str()))
{}

FILEPTR::~FILEPTR() 
{
   fclose(value);
}

#ifndef WIN32

FILEDES::FILEDES(int fd) 
   : GenericType<int> (fd)
{}

FILEDES::FILEDES(const string &filename, int mode) 
   : GenericType<int> (open(filename.c_str(), mode))
{}

FILEDES::~FILEDES() 
{
   close(value);
}


#endif /*ifdef WIN32*/
