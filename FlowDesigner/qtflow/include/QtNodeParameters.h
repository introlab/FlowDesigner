#ifndef _QTNODEPARAMETERS_H_
#define _QTNODEPARAMETERS_H_

#include "UINode.h"
#include "UINodeParameters.h"
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QTabWidget>
#include <QLineEdit>
#include <QComboBox>
#include <QGridLayout>

namespace FD {
    
    using namespace std;
    
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
        QGridLayout *m_paramsLayout;
		QTabWidget *m_tabWidget;
		QDialogButtonBox *m_buttonBox;
        UINodeParameters *m_params;
        QList<QWidget*> m_valuesWidge;
        QList<QComboBox*> m_typesWidge;
        
        protected:
        void addValues(int index, string type);
        void validParameters();     
        void setView(const std::vector<ItemInfo *> &textParams);
        
        protected slots:
        void buttonClicked( QAbstractButton * button );
        void valueChanged(); 
        void typeChanged();
        
	};
    
}//namespace FD


#endif
