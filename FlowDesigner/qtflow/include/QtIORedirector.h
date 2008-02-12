#ifndef _QTIOREDIRECTOR_
#define _QTIOREDIRECTOR_

#include <streambuf>
#include <QObject>

class QtIORedirector : public QObject, public std::streambuf
{

	Q_OBJECT;
	
public:
	
signals:
   void newOutput(const char*, std::streamsize);
	
protected:
	virtual std::streamsize xsputn ( const char * s, std::streamsize n )
	{
		emit newOutput(s,n);
		return n;
	}
	
};

#endif