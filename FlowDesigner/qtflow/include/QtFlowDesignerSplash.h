#ifndef _QTFLOWDESIGNERSPLASH_
#define _QTFLOWDESIGNERSPLASH_

#include <QSplashScreen>
#include <QString>

namespace FD
{

	class QtFlowDesignerSplash : public QSplashScreen
	{
		Q_OBJECT;
		
	public:
		
		QtFlowDesignerSplash();
		
		
	public slots:
		void displayMessage(QString message);
	};

}


#endif
