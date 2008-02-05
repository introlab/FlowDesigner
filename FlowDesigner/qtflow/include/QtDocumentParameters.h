#ifndef _QTDOCUMENTPARAMETERS_H_
#define _QTDOCUMENTPARAMETERS_H_

#include "UIDocument.h"
#include "UINodeParameters.h"
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QTabWidget>
#include <QLineEdit>
#include <QTextEdit>
#include <QComboBox>
#include <QGridLayout>
#include <vector>

namespace FD {
    
    using namespace std;
    
	class QtDocumentParameters : public QDialog
	{
		Q_OBJECT;
        
		public:
		
		QtDocumentParameters(UIDocument *doc);
		static std::vector<std::string> getObjectTypes();
		
		protected:
		
		QWidget* buildParametersTable();	
		UIDocument *m_doc;
		
		
		
		QVBoxLayout *m_vLayout;
        QGridLayout *m_paramsLayout;
		QTabWidget *m_tabWidget;
		QTextEdit *m_docComments;
		QLineEdit *m_docCategory;
		QDialogButtonBox *m_buttonBox;
        
		vector<ItemInfo*> m_params;
        
        QList<QWidget*> m_valuesWidge;
        QList<QComboBox*> m_typesWidge;
        
        protected:
        
        void addValues(int index, string type);
        void validParameters();     
        void setView(const std::vector<ItemInfo *> &textParams);
        void synchronizeParams(bool writeback);
        
        protected slots:
        void buttonClicked( QAbstractButton * button );
        void valueChanged(); 
        void typeChanged();
        
	};
    
}//namespace FD


#endif
