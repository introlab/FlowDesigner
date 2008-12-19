#ifndef _QTFLOW_H_
#define _QTFLOW_H_

#include <QThread>
#include "QtRunContext.h"
#include "QtDLManager.h"
#include "UIDocument.h"
#include "ParameterSet.h"
#include "ObjectRef.h"
#include "Network.h"
#include "Exception.h"
#include <sstream>
#include <iostream>
#include <string>
#include <QApplication>
#include <QThread>
#include <QTimer>
#include "QtProbeManager.h"
#include <QList>


namespace FD
{

	//Forward declaration
	class QtFlowApp;

	class QtFlowProcessingThread : public QThread
	{
		Q_OBJECT;

		protected:

		QtFlowApp *m_app;
		QtRunContext *m_context;

		virtual void run();

		public:

		QtFlowProcessingThread(QtFlowApp *app, UIDocument *doc, ParameterSet &params);
		virtual ~QtFlowProcessingThread();

	};

	class QtFlowApp : public QApplication
	{

		Q_OBJECT;

	public slots:



	protected:

		QList<QtFlowProcessingThread*> m_threadList;


	public:

		QtFlowApp(int argc, char* argv[]);
		virtual ~QtFlowApp();
	};

}//namespace FD

#endif

