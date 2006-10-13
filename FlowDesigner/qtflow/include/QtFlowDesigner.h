#ifndef _QTFLOWDESIGNER_H_
#define _QTFLOWDESIGNER_H_

#include <QtGui/QMainWindow>
#include <QtGui/QWidget>
#include <string>

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


#endif

