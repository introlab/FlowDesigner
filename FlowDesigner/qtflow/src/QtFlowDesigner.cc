//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#include "QtFlowDesigner.h"
#include "QtNetwork.h"

namespace FD
{

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

}//namespace FD

