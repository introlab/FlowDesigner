//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#ifndef _QTFLOWDESIGNER_H_
#define _QTFLOWDESIGNER_H_

#include <QtGui/QMainWindow>
#include <QtGui/QWidget>
#include <string>

namespace FD
{

class QtFlowDesigner : public QMainWindow
{
    Q_OBJECT
    
    public:
        
    QtFlowDesigner (QWidget* parent = NULL);
      
    virtual ~QtFlowDesigner();
    
    void newNetwork(const std::string name);
    
    public slots:
        void newNetworkClicked();         
};

}//namespace FD
#endif

