#ifndef _QTNODEPARAMETERS_H_
#define _QTNODEPARAMETERS_H_

#include "UINode.h"
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QTabWidget>

namespace FD {

	class QtNodeParameters : public QDialog
	{
		public:
		
		QtNodeParameters(UINode *node);
		
		protected:
		
		QVBoxLayout *m_vLayout;
		QTabWidget *m_tabWidget;
		QDialogButtonBox *m_buttonBox;

	};

}//namespace FD


#endif
