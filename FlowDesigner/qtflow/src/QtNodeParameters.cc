#include "QtNodeParameters.h"
#include <QTextEdit>
#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QComboBox>
#include <vector>
#include <map>
#include "UINodeParameters.h"
#include "Object.h"

namespace FD {

	using namespace std;
	

	
	std::vector<std::string> QtNodeParameters::getObjectTypes()
	{
		std::map<std::string, _ObjectFactory*>& factoryMap = Object::ObjectFactoryDictionary();
	
		std::vector<std::string> typeVect;
		
		for (std::map<std::string, _ObjectFactory*>::iterator iter = factoryMap.begin(); iter != factoryMap.end(); iter++)
		{
			typeVect.push_back(iter->first);
		}
		
		return typeVect;
	}
		
	
	QtNodeParameters::QtNodeParameters(UINode *node)
		:	m_node(node)
	{

		
		m_vLayout = new QVBoxLayout(this);
	
		m_tabWidget = new QTabWidget(this);
	




		m_tabWidget->addTab(buildParametersTable(),"Parameters");
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
	
	/*
	class ParameterText 
			{
				public:
				std::string name;
				std::string value;
				std::string type;	
				std::string description;
			};
	*/
	
	QWidget* QtNodeParameters::buildParametersTable()
	{
	
		QWidget *widget = new QWidget(this);
	
	
		QGridLayout *layout = new QGridLayout(widget);
		
			
		layout->addWidget(new QLabel("<b>NAME</b>",this),0,0,Qt::AlignLeft);
		layout->addWidget(new QLabel("<b>TYPE</b>",this),0,1,Qt::AlignLeft);
		layout->addWidget(new QLabel("<b>DESCRIPTION</b>",this),0,2,Qt::AlignLeft);
		layout->addWidget(new QLabel("<b>VALUE</b>",this),0,3,Qt::AlignLeft);
		
		std::vector<std::string> typeVect = QtNodeParameters::getObjectTypes();
	
		//GET PARAMETERS
		UINodeParameters *params  = m_node->getParameters();
		
		if (params)
		{
	
			
			std::vector<ParameterText *> &textParams = params->get_textParams();
			
			for (unsigned int i = 0; i < textParams.size(); i++)
			{
			
				layout->addWidget(new QLabel(textParams[i]->name.c_str(),this),i+1,0,Qt::AlignLeft);
				
				QComboBox *combo = new QComboBox(this);

				//Fill combo types
				for (unsigned int j = 0; j < typeVect.size(); j++)
				{
					combo->addItem(typeVect[j].c_str());
				}

				layout->addWidget(combo,i+1,1,Qt::AlignLeft);

				
				layout->addWidget(new QLabel(textParams[i]->description.c_str(),this),i+1,2,Qt::AlignLeft);
				layout->addWidget(new QLineEdit(textParams[i]->value.c_str(),this),i+1,3,Qt::AlignLeft);
			
				/*
				table->setCurrentCell(i,0);	
				table->setCurrentItem(new QTableWidgetItem(textParams[i]->name.c_str()));				
				table->setCurrentCell(i,1);
				table->setCurrentItem(new QTableWidgetItem(textParams[i]->value.c_str()));
				table->setCurrentCell(i,2);
				table->setCurrentItem(new QTableWidgetItem(textParams[i]->type.c_str()));
				table->setCurrentCell(i,3);						
				table->setCurrentItem(new QTableWidgetItem(textParams[i]->description.c_str()));
				*/
			}
		}
		
		
		
		return widget;
	}

} //namespace FD
