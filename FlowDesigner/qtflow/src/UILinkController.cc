#include "UILinkController.h"
#include "UITerminalController.h"
#include "QtNetwork.h"
#include "QtLink.h"
#include "QtNode.h"

namespace FD
{
	using namespace std;
    
    
	UILinkController::UILinkController ( UITerminalController *_from,UITerminalController *_to,const char *points_str )
    : UILink ( _from,_to,points_str ), m_QtLink ( NULL )
	{
	}
    
	void UILinkController::updateView ( QtNetwork *net )
	{
		if ( net )
		{
            
			//CREATE VIEW IF REQUIRED
			if ( !m_QtLink )
			{
				QtTerminal *source = dynamic_cast<UITerminalController*> ( from )->getQtTerminal();
				QtTerminal *dest = dynamic_cast<UITerminalController*> ( to )->getQtTerminal();
                
				//add link
				m_QtLink = net->addLink ( source,dest,this );
			}
			else
			{
				//ADJUST LINK
				m_QtLink->adjust();
			}
            
		}
	}
    
	void UILinkController::positionChanged ( double x, double y )
	{
		if ( m_QtLink )
		{
			m_QtLink->adjust();
		}
	}
    
    bool UILinkController::valid()
    {
        if(from->getType()=="any" || to->getType()=="any" )
        {
            return true;  
        }
        return (from->getType() == to->getType());  
    }
    
} //namespace FD
