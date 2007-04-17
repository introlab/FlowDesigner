#ifndef _UIDOCUMENTCONTROLLER_H_
#define _UIDOCUMENTCONTROLLER_H_

#include "UIDocument.h"
#include "QtDocument.h"
#include <QObject>

namespace FD
{
	class UIDocumentController : public QObject, public UIDocument
	{
		Q_OBJECT;
		
		public:
		
		UIDocumentController(const std::string &name, QtDocument *doc);
	
		//Inherited from UIDocument, must be reimplemented
		virtual UINetwork *newNetwork(const std::string &_name, UINetwork::Type type);   

		private :
		
		UIDocumentController();
		
		QtDocument *m_QtDocument;
	
	};

} //namespace FD

#endif


