// Copyright (C) 2008 Alberto Perotti
#ifndef _BIN2GRAY_H_
#define _BIN2GRAY_H_

namespace FD {

class bin2gray
{
public:
	bin2gray();
	virtual ~bin2gray();

	long const direct(long const bin) const
	{
		return bin ^ (bin >> 1);
	};

	long const inverse(long const gray) const
	{
		long bin  = gray;
		long i = 1;
		long d;

		while(1)
		{
			d = bin >> i;
			bin ^= d;
			if((d <= 1) || (i >= 16)) break;
			i <<= 1;
		}
		return bin;
	};
};	// class bin2gray

}	// namespace FD
#endif	// _BIN2GRAY_H_
