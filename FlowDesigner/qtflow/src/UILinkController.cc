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
                //CREATE VIEW IF REQUIRED
                if ( !m_QtLink && net )
                {
                        QtTerminal *source = dynamic_cast<UITerminalController*> ( from )->getQtTerminal();
                        QtTerminal *dest = dynamic_cast<UITerminalController*> ( to )->getQtTerminal();

                        //add link
                        m_QtLink = net->addLink ( source,dest,this );
                }
        }

    void UILinkController::positionChanged(double x, double y)
    {
        if (m_QtLink)
        {
            m_QtLink->setPos(x,y);
        }
    }

} //namespace FD
