#ifndef QTNETWORKSCENE_H_
#define QTNETWORKSCENE_H_

#include "QtNetwork.h"
#include <QGraphicsScene>
namespace FD
{

class QtNetworkScene : public QGraphicsScene
{
	Q_OBJECT;
	
	public:
		QtNetworkScene(QtNetwork *parent) : QGraphicsScene(parent), m_networkView(parent) {}
		
	protected:
		virtual void contextMenuEvent(QGraphicsSceneContextMenuEvent *contextMenuEvent);
		
	private:
		QtNetwork *m_networkView;
	
};

} //end namespace FD

#endif /*QTNETWORKSCENE_H_*/
