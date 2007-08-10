#include "QtNodeParameters.h"
#include <QTextEdit>
#include <QGridLayout>
#include <QLabel>

#include <QComboBox>
#include <QPushButton>
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
        
		m_params  = m_node->getParameters();
		m_vLayout = new QVBoxLayout(this);
        
		m_tabWidget = new QTabWidget(this);
        
		m_tabWidget->addTab(buildParametersTable(),"Parameters");
		m_tabWidget->addTab(new QTextEdit(this),"Comments");
		m_tabWidget->addTab(new QTextEdit(this),"Inputs/Outputs");
        
		m_vLayout->addWidget(m_tabWidget);
        
		m_buttonBox = new QDialogButtonBox(QDialogButtonBox::Apply | QDialogButtonBox::Cancel 
        | QDialogButtonBox::Ok | QDialogButtonBox::RestoreDefaults
        , Qt::Horizontal, this);
        
        m_buttonBox->button(QDialogButtonBox::Apply)->setDisabled(true); 
        
		m_vLayout->addWidget(m_buttonBox);
        
        connect(m_buttonBox, SIGNAL( clicked(QAbstractButton*) ), this, SLOT(buttonClicked( QAbstractButton*)) );
        
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
        
        /**************************/
        /**    Tab parameters    **/
        /**************************/
        
		QGridLayout *layout = new QGridLayout(widget);
		
        //Title
		layout->addWidget(new QLabel("<b>NAME</b>",this),0,0,Qt::AlignLeft | Qt::AlignTop);
		layout->addWidget(new QLabel("<b>TYPE</b>",this),0,1,Qt::AlignLeft | Qt::AlignTop);
		layout->addWidget(new QLabel("<b>DESCRIPTION</b>",this),0,2,Qt::AlignLeft | Qt::AlignTop);
		layout->addWidget(new QLabel("<b>VALUE</b>",this),0,3,Qt::AlignLeft | Qt::AlignTop);
		
		
		if (m_params)
		{
			std::vector<ParameterText *> &textParams = m_params->get_textParams();
			m_valuesWidge =  new QWidget*[textParams.size()];
            
            
			for (unsigned int i = 0; i < textParams.size(); i++)
			{
                
				//ADD NAME
                
                layout->addWidget(new QLabel(textParams[i]->name.c_str(),this),i+1,0,Qt::AlignLeft | Qt::AlignTop);
				
                //ADD TYPE
				
                if(m_params->get_defaultTextParams()[i]->type == "any")
                {               
                    //Fill combo for "any" types
                    std::vector<std::string> typeVect = QtNodeParameters::getObjectTypes();
                    QComboBox *combo = new QComboBox(this);
                    for (unsigned int j = 0; j < typeVect.size(); j++)
                    {
                        combo->addItem(typeVect[j].c_str());
                        //problem minuscule majuscule 1er lettre
                        if(textParams[i]->type.substr(1) == typeVect[j].substr(1))
                        {
                            combo->setCurrentIndex( j );
                        }
                    }
                    layout->addWidget(combo,i+1,1,Qt::AlignLeft);
                }
                else
                {
                    layout->addWidget(new QLabel(textParams[i]->type.c_str(),this),i+1,1,Qt::AlignLeft | Qt::AlignTop);
                }
                
                //ADD DESCRIPTION
				
				layout->addWidget(new QLabel(textParams[i]->description.c_str(),this),i+1,2,Qt::AlignLeft | Qt::AlignTop);
                
                //ADD VALUE
                
                if(textParams[i]->type == "string")
                {
                    string defaultString = m_params->get_defaultTextParams()[i]->value;
                    QComboBox* combo =  new QComboBox(this);
                    combo->setAccessibleName("QComboBox");
                    unsigned int lastPos=0;
                    
                    ////Fill combo with string like : "value1;value2;value3"
                    for(unsigned int j = 0;j < defaultString.size(); j++)
                    {
                        if(defaultString[j]==';')
                        {
                            combo->addItem(defaultString.substr(lastPos,j-lastPos).c_str());                     
                            lastPos = j+1;
                        }
                    }               
                    if(lastPos!=0)
                    {
                        combo->addItem(defaultString.substr(lastPos).c_str());                 
                        combo->setCurrentIndex( combo->findText( textParams[i]->value.c_str(), Qt::MatchExactly));
                        connect(combo, SIGNAL( currentIndexChanged(int) ), this, SLOT( viewChanged() ) );
                        m_valuesWidge[i] = combo;
                    }
                    //if no ';' like : "value"
                    else
                    {
                        QLineEdit* lineEdit = new QLineEdit(textParams[i]->value.c_str(),this);
                        connect(lineEdit, SIGNAL( textChanged(const QString & ) ), this, SLOT( viewChanged() ) );
                        lineEdit->setAccessibleName("QLineEdit");
                        m_valuesWidge[i] = lineEdit;                
                    }                              
                }
                //if value isn't a tring
                else
                {
                    QLineEdit* lineEdit = new QLineEdit(textParams[i]->value.c_str(),this);
                    connect(lineEdit, SIGNAL( textChanged(const QString & ) ), this, SLOT( viewChanged() ) );
                    lineEdit->setAccessibleName("QLineEdit");
                    m_valuesWidge[i] = lineEdit;
                }            
				layout->addWidget(m_valuesWidge[i],i+1,3,Qt::AlignLeft | Qt::AlignTop);
			}
		}		
		return widget;
	}
    
    void QtNodeParameters::validParameters()
    {
        cerr<<"QtNodeParameters::validParameters()"<<endl;
        if (m_params)
		{
            std::vector<ParameterText *> &textParams = m_params->get_textParams();
            for (unsigned int i = 0; i < textParams.size(); i++)
            {
                if(m_valuesWidge[i]->accessibleName() == "QLineEdit")
                    textParams[i]->value = dynamic_cast<QLineEdit *>(m_valuesWidge[i])->text().toStdString(); 
                else if(m_valuesWidge[i]->accessibleName() == "QComboBox")
                    textParams[i]->value = dynamic_cast<QComboBox *>(m_valuesWidge[i])->currentText().toStdString(); 
            }  
        }
    }
    
    void QtNodeParameters::buttonClicked (QAbstractButton * button )
    {
        cerr<<"QtNodeParameters::buttonClicked()"<<endl;
        if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::Apply ))
        {
            validParameters();
            m_buttonBox->button(QDialogButtonBox::Apply)->setDisabled(true); 
        }
        else if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::Ok ))
        {
            validParameters();
            close();
        }
        else  if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::RestoreDefaults ))
        { 
            setViewToDefault();
        }
        else
        {
            cerr<<"QtNodeParameters::buttonClicked() : unknown button"<<endl;
        }
    }
    void QtNodeParameters::setViewToDefault()
    {
        for (unsigned int i = 0; i < m_params->get_defaultTextParams().size(); i++)
        {
            if(m_valuesWidge[i]->accessibleName() == "QLineEdit")
                dynamic_cast<QLineEdit *>(m_valuesWidge[i])->setText( m_params->get_defaultTextParams()[i]->value.c_str() ); 
            else if(m_valuesWidge[i]->accessibleName() == "QComboBox")
                dynamic_cast<QComboBox *>(m_valuesWidge[i])->setCurrentIndex(0);
        }
    }
    
    void QtNodeParameters::viewChanged()
    {
        cerr<<"QtNodeParameters::viewChanged()"<<endl;
        m_buttonBox->button(QDialogButtonBox::Apply)->setEnabled(true); 
    }
    
} //namespace FD
