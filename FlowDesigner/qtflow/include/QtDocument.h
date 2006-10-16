#ifndef _QTDOCUMENT_H_
#define _QTDOCUMENT_H_

#include <QtGui/QWidget>
#include <QtGui/QMainWindow>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include "UIDocument.h"

namespace FD
{
    
    
    class QtDocument : public QMainWindow
    {         
    
        Q_OBJECT;

        public:
            QtDocument(QWidget *parent, const std::string &name = "Untitled");              
    
    
        protected:
            QVBoxLayout *m_vboxLayout;
            std::string m_name;
            QTabWidget *m_tabWidget;         
            UIDocument *m_doc;                 
    
    };
} //namespace FD  
#endif
