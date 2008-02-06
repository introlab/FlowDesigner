#include "QtDocumentParameters.h"
#include <QTextEdit>

#include <QDoubleSpinBox>
#include <QSpinBox>
#include <QLabel>


#include <QPushButton>
#include <vector>
#include <map>


#include "Object.h"
#include "UINodeRepository.h"
#include <QInputDialog>
#include <sstream>

namespace FD {
    
	std::vector<std::string> QtDocumentParameters::getObjectTypes()
	{
		std::map<std::string, _ObjectFactory*>& factoryMap = Object::ObjectFactoryDictionary();
        
		std::vector<std::string> typeVect;
		
		for (std::map<std::string, _ObjectFactory*>::iterator iter = factoryMap.begin(); iter != factoryMap.end(); iter++)
		{
			typeVect.push_back(iter->first);
		}
		
		return typeVect;
	}
    
	
	QtDocumentParameters::QtDocumentParameters(UIDocument *doc)
    :	m_doc(doc)
  	{
        
		assert(m_doc);
		
		UINetwork *net = m_doc->getNetworkNamed("MAIN");
		
		
		
		if (net)
		{

			//This will copy MAIN parameters into m_params
			net->insertNetParams(m_params);
			
			synchronizeParams(false);
			
			//Creating layout
			m_vLayout = new QVBoxLayout(this);
			m_tabWidget = new QTabWidget(this);
			
			//TODO ADD CATEGORY
			m_vLayout->addWidget(new QLabel("Category",this));
			m_docCategory = new QLineEdit(this);
			m_docCategory->setText(m_doc->getCategory().c_str());
			m_vLayout->addWidget(m_docCategory);
        
			//Tab widget
			m_tabWidget->addTab(buildParametersTable(), "Document Parameters");
			
			m_docComments = new QTextEdit(this);
			m_tabWidget->addTab(m_docComments,"Document Comments");
			m_vLayout->addWidget(m_tabWidget);
        
			
			//Set Document comments
			m_docComments->setText(m_doc->getComments().c_str());
			
			//Creating butons
			m_buttonBox = new QDialogButtonBox(QDialogButtonBox::Cancel 
					| QDialogButtonBox::Ok, Qt::Horizontal, this);
        
		
			//Adding buttons to layout
			m_vLayout->addWidget(m_buttonBox);
        
			connect(m_buttonBox, SIGNAL( clicked(QAbstractButton*) ), this, SLOT(buttonClicked( QAbstractButton*)) );
        
			//set layout & resize dialog
			setLayout(m_vLayout);
			resize(640,480);
		}
	}

	void QtDocumentParameters::synchronizeParams(bool writeback)
	{
		if (m_doc)
		{
			std::vector<ItemInfo*> docInfo = m_doc->get_textParams();
			
			//Go through all known parameters
			for (unsigned int i = 0; i < m_params.size(); i++)
			{
				bool found = false;
				unsigned int index = 0;
				
				
				for (unsigned int j = 0; j < docInfo.size(); j++)
				{
					if (docInfo[j]->name == m_params[i]->name)
					{
						index = j;
						found = true;
						break;
					}
				}
				
				if (found)
				{
					if (writeback)
					{
						//UPDATE DOCUMENT FROM PARAMS
						docInfo[index]->type = m_params[i]->type;
						docInfo[index]->value = m_params[i]->value;
						docInfo[index]->description = m_params[i]->description;
					}
					else
					{
						//UPDATE PARAMS FROM DOCUMENT
						m_params[i]->type = docInfo[index]->type;
						m_params[i]->value = docInfo[index]->value;
						m_params[i]->description = docInfo[index]->description;
					}
				}
				else
				{
					//ADD PARAM
					m_doc->addParameterText(m_params[i]->name, m_params[i]->value,m_params[i]->type,m_params[i]->description);
				}
			}
		}
	}
	
	
	QWidget* QtDocumentParameters::buildParametersTable()
	{
		QWidget *widget = new QWidget(this);
        
        /**************************/
        /**    Tab parameters    **/
        /**************************/
        
		m_paramsLayout = new QGridLayout(widget);
		
        //Titles
		m_paramsLayout->addWidget(new QLabel("<b>NAME</b>",this),0,0,Qt::AlignLeft | Qt::AlignTop);
		m_paramsLayout->addWidget(new QLabel("<b>TYPE</b>",this),0,1,Qt::AlignLeft | Qt::AlignTop);
		m_paramsLayout->addWidget(new QLabel("<b>DESCRIPTION</b>",this),0,2,Qt::AlignLeft | Qt::AlignTop);
		m_paramsLayout->addWidget(new QLabel("<b>VALUE</b>",this),0,3,Qt::AlignLeft | Qt::AlignTop);
		

		for (unsigned int i = 0; i < m_params.size(); i++)
		{
			//ADD NAME
            m_paramsLayout->addWidget(new QLabel(m_params[i]->name.c_str(),this),i+1,0,Qt::AlignLeft | Qt::AlignTop);
			
            //ADD TYPE
			
            if(m_params[i]->type == "any" ||
               m_params[i]->type == "subnet_param")
            {                                   
                //Fill combo for "any" types
                std::vector<std::string> typeVect = QtDocumentParameters::getObjectTypes();
                QComboBox *combo = new QComboBox(this);
                combo->addItem("int");
                combo->addItem("string");
                combo->addItem("float");
                combo->addItem("bool");
                combo->addItem("object");
                
                int index = combo->findText(m_params[i]->type.c_str());
                if( index != -1)
                    combo->setCurrentIndex(index);
                else                  
                    combo->setCurrentIndex(0);
                
         
                
                m_paramsLayout->addWidget(combo,i+1,1,Qt::AlignLeft);
                connect(combo, SIGNAL( currentIndexChanged(int) ), this, SLOT( typeChanged() ) );
                //The name is used to associate the comnbo with the value :
                combo->setAccessibleName(QString::number(i));
                m_typesWidge.append(combo); 
            }
            else
            {
                m_paramsLayout->addWidget(new QLabel(m_params[i]->type.c_str(),this),i+1,1,Qt::AlignLeft | Qt::AlignTop);
            }
            
            //ADD DESCRIPTION
			
			m_paramsLayout->addWidget(new QLabel(m_params[i]->description.c_str(),this),i+1,2,Qt::AlignLeft | Qt::AlignTop);
            
            //ADD VALUE
            addValues(i,m_params[i]->type);
            
		}
				
		return widget;
	}
    
    
    //ADD different objects to  m_valuesWidge[index] by using the type of m_params->get_textParams()[index]->type
    //string -> QComboBox (value1;value2...) QLineEdit (value?)
    //bool   -> QComboBox (true or false)
    //int    -> QSpinBox
    //float  -> QDoubleSpinBox
    //other  -> QLineEdit
    
    void QtDocumentParameters::addValues(int index, string type)    
    {

        if( index < m_valuesWidge.size() ) 
        {
            m_paramsLayout->removeWidget( m_valuesWidge[index] );
            delete m_valuesWidge.takeAt(index);            
        }
        
        unsigned int lastPos=0;
        if(type == "string")
        {
            string defaultString = m_params[index]->value;
            QComboBox* combo =  new QComboBox(this);
            combo->setAccessibleName("QComboBox");
            
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
                combo->setCurrentIndex( combo->findText(m_params[index]->value.c_str(), Qt::MatchExactly));
                connect(combo, SIGNAL( currentIndexChanged(int) ), this, SLOT( valueChanged() ) );
                m_valuesWidge.insert(index, combo);
            }   
            else
            {
                delete combo;
                QLineEdit* lineEdit = new QLineEdit(m_params[index]->value.c_str(),this);
                connect(lineEdit, SIGNAL( textChanged(const QString & ) ), this, SLOT( valueChanged() ) );
                lineEdit->setAccessibleName("QLineEdit");
                m_valuesWidge.insert(index, lineEdit);
            }
            
        }
        else if(type == "bool")
        {
            QComboBox* combo =  new QComboBox(this);
            combo->setAccessibleName("QComboBox");
            combo->addItem("true");
            combo->addItem("false");
            combo->setCurrentIndex( combo->findText(m_params[index]->value.c_str(), Qt::MatchExactly));
            connect(combo, SIGNAL( currentIndexChanged(int) ), this, SLOT( valueChanged() ) );
            m_valuesWidge.insert(index, combo);                    
        }
        else if(type == "int")
        {
            QSpinBox* spinBox =  new QSpinBox(this);
            spinBox->setAccessibleName("QSpinBox");       
            //Maybe to change... extremum value of int generate a warning
            spinBox->setRange ( -1000000, 1000000 );
            spinBox->setValue( QString(m_params[index]->value.c_str()).toInt()  ) ; 
            connect(spinBox, SIGNAL( valueChanged(int) ), this, SLOT( valueChanged() ) );
            m_valuesWidge.insert(index, spinBox);                    
        }
        else if(type == "float")
        {
            QDoubleSpinBox* doubleSpinBox =  new QDoubleSpinBox(this);
            doubleSpinBox->setAccessibleName("QDoubleSpinBox");
            //Maybe to change...
            doubleSpinBox->setRange( -1000000, 1000000 );
            doubleSpinBox->setValue( QString(m_params[index]->value.c_str()).toFloat()  ) ; 
            connect(doubleSpinBox, SIGNAL( valueChanged(double) ), this, SLOT( valueChanged() ) );
            m_valuesWidge.insert(index, doubleSpinBox);                    
        }
        else
        {
            QLineEdit* lineEdit = new QLineEdit(m_params[index]->value.c_str(),this);
            connect(lineEdit, SIGNAL( textChanged(const QString & ) ), this, SLOT( valueChanged() ) );
            lineEdit->setAccessibleName("QLineEdit");
            m_valuesWidge.insert(index, lineEdit);
        }
        m_paramsLayout->addWidget(m_valuesWidge[index],index+1,3,Qt::AlignLeft | Qt::AlignTop);
    }
    
    void QtDocumentParameters::validParameters()
    {
 
        //Valid Types
        for (unsigned int i = 0; i < m_typesWidge.size(); i++)
        {
            int paramsIndex = m_typesWidge[i]->accessibleName().toInt();
            m_params[paramsIndex]->type = m_typesWidge[i]->currentText().toStdString();
        }
        //Valid Values
        for (unsigned int i = 0; i < m_params.size(); i++)
        {
            if(m_valuesWidge[i]->accessibleName() == "QLineEdit")
                m_params[i]->value = dynamic_cast<QLineEdit *>(m_valuesWidge[i])->text().toStdString(); 
            else if(m_valuesWidge[i]->accessibleName() == "QComboBox")
                m_params[i]->value = dynamic_cast<QComboBox *>(m_valuesWidge[i])->currentText().toStdString(); 
            else if(m_valuesWidge[i]->accessibleName() == "QSpinBox")
                m_params[i]->value = dynamic_cast<QSpinBox *>(m_valuesWidge[i])->text().toStdString(); 
            else if(m_valuesWidge[i]->accessibleName() == "QDoubleSpinBox")
                m_params[i]->value = dynamic_cast<QDoubleSpinBox *>(m_valuesWidge[i])->text().toStdString(); 
        }
        
        //Sync data with existing document parameters
        synchronizeParams(true);
        
        //UPDATE CATEGORY
        m_doc->setCategory(m_docCategory->text().toStdString());
        
        //UPDATE COMMENTS
        m_doc->setComments(m_docComments->toPlainText().toStdString());
        
    }
    
    void QtDocumentParameters::buttonClicked (QAbstractButton * button )
    {

    	if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::Ok ))
        {
        	validParameters();
            close();
        }
        else if(button == (QAbstractButton*)m_buttonBox->button( QDialogButtonBox::Cancel ))
        {
            setView(m_params);
            close();
        }
        else
        {
            cerr<<"QtDocumentParameters::buttonClicked() : unknown button"<<endl;
        }
    }
    
    //Set the parameters view with parameters give 
    void QtDocumentParameters::setView(const std::vector<ItemInfo *> &textParams)
    {
        cerr<<"QtDocumentParameters::setView()"<<endl;
        //set type
        for (unsigned int i = 0; i < m_typesWidge.size(); i++)
        {
            int paramsIndex = m_typesWidge[i]->accessibleName().toInt();
            int index = m_typesWidge[i]->findText( textParams[paramsIndex]->type.c_str() );
            if( index != -1)
                m_typesWidge[i]->setCurrentIndex(index);
            else       
            {
                m_typesWidge[i]->setCurrentIndex(0);
            } 
        }
        //set Value
        for (unsigned int i = 0; i < textParams.size(); i++)
        {
            //Update Values View
            if(m_valuesWidge[i]->accessibleName() == "QLineEdit")
                dynamic_cast<QLineEdit *>(m_valuesWidge[i])->setText( textParams[i]->value.c_str() ); 
            else if(m_valuesWidge[i]->accessibleName() == "QSpinBox")
                dynamic_cast<QSpinBox *>(m_valuesWidge[i])->setValue( QString(textParams[i]->value.c_str()).toInt()  ) ; 
            else if(m_valuesWidge[i]->accessibleName() == "QDoubleSpinBox")
                dynamic_cast<QDoubleSpinBox *>(m_valuesWidge[i])->setValue( QString(textParams[i]->value.c_str()).toInt()  ) ; 
            else if(m_valuesWidge[i]->accessibleName() == "QComboBox")
            {
                QComboBox *comboBox = dynamic_cast<QComboBox *>(m_valuesWidge[i]);
                int index = comboBox->findText( textParams[i]->value.c_str() );
                if( index != -1)
                    comboBox->setCurrentIndex(index);
                else       
                {
                    comboBox->setCurrentIndex(0);
                }
            } 
        }
    }
    
    void QtDocumentParameters::valueChanged()
    {
        cerr<<"QtDocumentParameters::valueChanged()"<<endl;
    } 
    
    void QtDocumentParameters::typeChanged()
    {
        cerr<<"QtDocumentParameters::typeChanged()"<<endl;
        for (unsigned int i = 0; i < m_typesWidge.size(); i++)
        {
            int paramsIndex = m_typesWidge[i]->accessibleName().toInt();
            addValues(paramsIndex, m_typesWidge[i]->currentText().toStdString());
        }
    }
    
} //namespace FD