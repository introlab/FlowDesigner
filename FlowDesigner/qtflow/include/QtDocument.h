#ifndef _QTDOCUMENT_H_
#define _QTDOCUMENT_H_

#include <QtGui/QWidget>
#include <QtGui/QMainWindow>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include "UIDocument.h"


namespace FD
{
    
    //forward declaration
    class QtNetwork;
           
    class QtDocument : public QMainWindow
    {         
    
        Q_OBJECT;

        public:
            QtDocument(QWidget *parent, const std::string &name = "Untitled");
                          
            void open(const std::string &file);         
    
    
        protected:
            QVBoxLayout *m_vboxLayout;
            std::string m_name;
            QTabWidget *m_tabWidget;         
            UIDocument *m_doc;
                             
            std::vector<QtNetwork*> m_networks;

    
    };
} //namespace FD  
#endif
