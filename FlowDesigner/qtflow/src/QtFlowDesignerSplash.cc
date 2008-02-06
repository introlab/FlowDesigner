#include "QtFlowDesignerSplash.h"
#include <iostream>

namespace FD
{
	using namespace std;

	QtFlowDesignerSplash::QtFlowDesignerSplash()
		: QSplashScreen(NULL, QPixmap(), Qt::WindowStaysOnTopHint)
	{
		//Set the pixmap
		QPixmap pixmap;	
		pixmap.load(QString(INSTALL_PREFIX) + QString("/share/flowdesigner/FlowDesignerSplashScreen.jpg"));
		setPixmap(pixmap);
		resize(pixmap.width(), pixmap.height());
		//Start the timer that will close the splash screen
		startTimer(5000);
	}

	void QtFlowDesignerSplash::displayMessage(QString message)
	{
		cerr<<"Got QtFlowDesignerSplash::displayMessage(QString message)"<<endl;
		showMessage(QString("Loading : ") + message);
	}
	
	void QtFlowDesignerSplash::timerEvent(QTimerEvent *event)
	{
	     close();
	}
	
} //FD namespace

