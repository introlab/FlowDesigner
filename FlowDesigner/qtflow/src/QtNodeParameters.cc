#include "QtNodeParameters.h"
#include <QTextEdit>

namespace FD {

	QtNodeParameters::QtNodeParameters(UINode *node)
	{

		m_vLayout = new QVBoxLayout(this);
	
		m_tabWidget = new QTabWidget(this);
	




		m_tabWidget->addTab(new QTextEdit(this),"Parameters");
		m_tabWidget->addTab(new QTextEdit(this),"Comments");
		m_tabWidget->addTab(new QTextEdit(this),"Inputs/Outputs");




	
		m_vLayout->addWidget(m_tabWidget);
	
	
		m_buttonBox = new QDialogButtonBox(QDialogButtonBox::Apply | QDialogButtonBox::Cancel | QDialogButtonBox::Ok,
						Qt::Horizontal, this);
						
		m_vLayout->addWidget(m_buttonBox);
	
		//TODO CONNECT BUTTONS
	
	
		setLayout(m_vLayout);
		
		resize(640,480);
		
	
	}

} //namespace FD
