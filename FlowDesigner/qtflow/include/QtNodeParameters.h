#ifndef _QTNODEPARAMETERS_H_
#define _QTNODEPARAMETERS_H_

#include "UINode.h"
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QTabWidget>
#include <QLineEdit>
namespace FD {
    
	class QtNodeParameters : public QDialog
	{
		Q_OBJECT;
        
		public:
		
		QtNodeParameters(UINode *node);
		static std::vector<std::string> getObjectTypes();
		
		protected:
		
		QWidget* buildParametersTable();		
		UINode *m_node;
		QVBoxLayout *m_vLayout;
		QTabWidget *m_tabWidget;
		QDialogButtonBox *m_buttonBox;
        UINodeParameters *m_params;
        QWidget **m_valuesWidge;
        QWidget **m_typesWidge;
        
        protected:
        void validParameters();     
        void setViewToDefault();      
        
        protected slots:
        void buttonClicked( QAbstractButton * button );
        void viewChanged();
	};
    
}//namespace FD


#endif
