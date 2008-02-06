#include "QtFlowDesignerSplash.h"


namespace FD
{
	QtFlowDesignerSplash::QtFlowDesignerSplash()
		: QSplashScreen(NULL, QPixmap(), Qt::WindowStaysOnTopHint)
	{
		//Set the pixmap
		QPixmap pixmap;
		
		pixmap.load(QString(INSTALL_PREFIX) + QString("/share/flowdesigner/FlowDesignerSplashScreen.jpg"));
		
		setPixmap(pixmap);
		
		resize(pixmap.width(), pixmap.height());
		
		startTimer(5000);
	}

	void QtFlowDesignerSplash::displayMessage(QString message)
	{
		showMessage(QString("Loading : ") + message);
	}
	
	void QtFlowDesignerSplash::timerEvent(QTimerEvent *event)
	{
	     close();
	}
	
} //FD namespace

