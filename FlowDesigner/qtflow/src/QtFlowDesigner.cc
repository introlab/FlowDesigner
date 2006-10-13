#include "QtFlowDesigner.h"
#include "QtNetwork.h"


QtFlowDesigner::QtFlowDesigner (QWidget* parent)
    : QMainWindow(parent)
{
    
     //setupUi(this);
    //setupUi(this);
    
    //connect(actionNewNetwork, SIGNAL(triggered()),this,SLOT(newNetworkClicked()));
}   
      
QtFlowDesigner::~QtFlowDesigner()
{
          
}



void QtFlowDesigner::newNetwork(const std::string name)
{
       
    //tabWidget->addTab (new Network(), name.c_str());

}

void QtFlowDesigner::newNetworkClicked()
{
    //newNetwork("TESTING");         
}

