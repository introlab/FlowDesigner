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
	}

	void QtFlowDesignerSplash::displayMessage(QString message)
	{
		cerr<<"Got QtFlowDesignerSplash::displayMessage(QString message)"<<endl;
		showMessage(QString("Loading : ") + message);
	}
	
} //FD namespace

