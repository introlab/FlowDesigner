#include "UITerminalController.h"
#include "UINetTerminalController.h"
#include "UINodeController.h"
#include "UILinkController.h"
#include "QtNode.h"
#include "QtTerminal.h"
#include <iostream>

namespace FD
{
	using namespace std;


	UITerminalController::UITerminalController ( ItemInfo *terminalInfo, UINodeController *_node,
	        bool _isInput, double _x, double _y )
			: UITerminal ( terminalInfo,_node,_isInput,_x,_y ), m_QtTerminal ( NULL )

	{
		//UPDATE VIEW(S)
		if ( terminalInfo && _node )
		{

			QtNode* qtNode = _node->getQtNode();

			if ( qtNode )
			{
				updateView ( qtNode );
			}
			else
			{
				cerr<<"WARNING : UITerminalController::UITerminalController -- No QtNode defined."<<endl;
			}
		}
	}

	QtTerminal* UITerminalController::getQtTerminal()
	{
		return m_QtTerminal;
	}

	void UITerminalController::updateView ( QtNode *node )
	{

		if ( node )
		{
			//CREATE VIEW IF REQUIRED
			if ( !m_QtTerminal )
			{
				m_QtTerminal = node->addTerminal ( this );

				cerr<<"CONNECTION TERMINAL CONTROLLER"<<endl;

				//connect signals
				QObject::connect ( m_QtTerminal,SIGNAL ( positionChanged ( float,float ) ),this,SLOT ( setPos ( float,float ) ) );

			}

			//UPDATE NET TERMINAL
			if ( netTerminal )
			{
				UINetTerminalController *netTerminalCTRL = dynamic_cast<UINetTerminalController*> ( netTerminal );

				if ( netTerminalCTRL )
				{
					netTerminalCTRL->updateView ( m_QtTerminal );
				}
			}

			//UPDATE LINKS
			for ( unsigned int i= 0; i < connections.size(); i++ )
			{

				UILinkController *linkCtrl = dynamic_cast<UILinkController*> ( connections[i] );

				if ( linkCtrl )
				{
					linkCtrl->updateView ( node->getQtNetwork() );
				}
				else
				{
					cerr<<"UITerminalController::updateView(QtNode *node) -- ERROR linkCtrl is NULL"<<endl;
				}

			}
		} //if node
	}

	void UITerminalController::setPos ( float _x, float _y )
	{
		cerr<<"(SLOT) void UITerminalController::setPos(float _x, float _y)"<<endl;
		x = _x;
		y = _y;
	}

} //namespace FD


