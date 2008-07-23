#include <QtGui>
#include <QLabel>
#include <QFile>
#include <QMessageBox>
#include <QSvgGenerator>
#include <QGraphicsSvgItem>
#include <QSvgRenderer>

#include "iconeditor/QtIconEditor.h"
#include "iconeditor/QtIconItem.h"
#include "iconeditor/QtIconScene.h"

#include <iostream>
#include <stdlib.h>

const int InsertTextButton = 10;

QtIconEditor::QtIconEditor(const QString &iconName, QWidget* parent) : QMainWindow(parent, Qt::Dialog)
{
    createActions();
    createToolBox();

    scene = new QtIconScene(this);
    scene->setSceneRect(QRectF(0, 0, SCENE_SIZE, SCENE_SIZE));
    connect(scene, SIGNAL(itemInserted()),
            this, SLOT(itemInserted()));
    connect(scene, SIGNAL(itemInserting(QGraphicsItem*)),
            this, SLOT(bringToFront(QGraphicsItem*)));
        
    createToolbars();
    createButtonBox();

	QVBoxLayout * vLayout = new QVBoxLayout;

    QHBoxLayout *hLayout = new QHBoxLayout;
    hLayout->addWidget(toolBox);
    view = new QGraphicsView(scene);
    hLayout->addWidget(view);

	vLayout->addLayout(hLayout);
	
	vLayout->addWidget(m_buttonBox);

    QWidget *widget = new QWidget;
    widget->setLayout(vLayout);

    setCentralWidget(widget);
    setWindowTitle(tr("Icon editor"));
    
    // Delete on close
	setAttribute(Qt::WA_DeleteOnClose);
	// Block input parent windows and always on top
    setWindowModality(Qt::WindowModal);
    
    resize(800,600);
    
    m_iconName = iconName; 
    // TODO: See how to load a SVG and save it like a SVG and not a simple picture...
    /*QString userIcon = QDir::homePath() + QDir::separator() + QString(".flowdesigner") + QDir::separator() + "icons" + QDir::separator() + m_iconName + ".svg";   
    if(QFile::exists(userIcon)) {
    	// Load the icon file
    	QGraphicsSvgItem* item = new QGraphicsSvgItem(userIcon);
    	item->scale(SCENE_SIZE/item->boundingRect().width(), SCENE_SIZE/item->boundingRect().height());
    	item->setFlag(QGraphicsItem::ItemIsMovable);
    	item->setFlag(QGraphicsItem::ItemIsSelectable);
    	scene->addItem(item);
    }*/
    
    setModified(false);

}

void QtIconEditor::buttonGroupClicked(int id)
{
    QList<QAbstractButton *> buttons = buttonGroup->buttons();
    foreach (QAbstractButton *button, buttons) {
	    if (buttonGroup->button(id) != button)
	        button->setChecked(false);
    }
    if(id == 0) {
    	scene->setMode(QtIconScene::InsertRectItem);
    }
    else if(id == 1) {
    	scene->setMode(QtIconScene::InsertLineItem);
    }
    else if(id == 2) {
    	scene->setMode(QtIconScene::InsertEllipseItem);
    }
    else if(id == 3) {
    	scene->setMode(QtIconScene::InsertPolygonItem);
    }
    else { //default
    	scene->setMode(QtIconScene::InsertRectItem);
    }
}

void QtIconEditor::deleteItem()
{
    foreach (QGraphicsItem *item, scene->selectedItems()) {
        scene->removeItem(item);
    }
    setModified(true);
}

void QtIconEditor::pointerGroupClicked(int)
{
    scene->setMode(QtIconScene::Mode(pointerTypeGroup->checkedId()));
}

void QtIconEditor::bringToFront(QGraphicsItem* item)
{
	if(item)
	{
		QList<QGraphicsItem *> overlapItems = item->collidingItems();

	    qreal zValue = 0;
	    foreach (QGraphicsItem *item, overlapItems) {
	        if (item->zValue() >= zValue)
	            zValue = item->zValue() + 0.1;
	    }
	    item->setZValue(zValue);
	    setModified(true);
	}
}

void QtIconEditor::bringToFront()
{
    if (scene->selectedItems().isEmpty())
        return;

    QGraphicsItem *selectedItem = scene->selectedItems().first();
    QList<QGraphicsItem *> overlapItems = selectedItem->collidingItems();

    qreal zValue = 0;
    foreach (QGraphicsItem *item, overlapItems) {
        if (item->zValue() >= zValue)
            zValue = item->zValue() + 0.1;
    }
    selectedItem->setZValue(zValue);
    setModified(true);
}

void QtIconEditor::sendToBack()
{
    if (scene->selectedItems().isEmpty())
        return;

    QGraphicsItem *selectedItem = scene->selectedItems().first();
    QList<QGraphicsItem *> overlapItems = selectedItem->collidingItems();

    qreal zValue = 0;
    foreach (QGraphicsItem *item, overlapItems) {
        if (item->zValue() <= zValue)
            zValue = item->zValue() - 0.1;
    }
    selectedItem->setZValue(zValue);
    setModified(true);
}

void QtIconEditor::itemInserted()
{
    scene->setMode(QtIconScene::Mode(pointerTypeGroup->checkedId()));
    
    QList<QAbstractButton *> buttons = buttonGroup->buttons();
    foreach (QAbstractButton *button, buttons) {
		button->setChecked(false);
    }
    
    setModified(true);
}

void QtIconEditor::sceneScaleChanged(const QString &scale)
{
    double newScale = scale.left(scale.indexOf(tr("%"))).toDouble() / 100.0;
    QMatrix oldMatrix = view->matrix();
    view->resetMatrix();
    view->translate(oldMatrix.dx(), oldMatrix.dy());
    view->scale(newScale, newScale);
}

void QtIconEditor::itemColorChanged()
{
    fillAction = qobject_cast<QAction *>(sender());
    fillColorToolButton->setIcon(createColorToolButtonIcon(
                 ":/images/floodfill.png",
                 qVariantValue<QColor>(fillAction->data())));
    fillButtonTriggered();
}

void QtIconEditor::lineColorChanged()
{
    lineAction = qobject_cast<QAction *>(sender());
    lineColorToolButton->setIcon(createColorToolButtonIcon(
                 ":/images/linecolor.png",
                 qVariantValue<QColor>(lineAction->data())));
    lineButtonTriggered();
}

void QtIconEditor::fillButtonTriggered()
{
    if(scene->setItemColor(qVariantValue<QColor>(fillAction->data()))) {
    	setModified(true);
    }
}

void QtIconEditor::lineButtonTriggered()
{
    if(scene->setLineColor(qVariantValue<QColor>(lineAction->data()))) {
    	setModified(true);	
    }
}

void QtIconEditor::createToolBox()
{
    buttonGroup = new QButtonGroup;
    buttonGroup->setExclusive(false);
    connect(buttonGroup, SIGNAL(buttonClicked(int)),
            this, SLOT(buttonGroupClicked(int)));
    QGridLayout *layout = new QGridLayout;
    layout->addWidget(createCellWidget(tr("Rectangle"),
                      QtIconItem::Rectangle), 0, 0);
    layout->addWidget(createCellWidget(tr("Line"),
                      QtIconItem::Line), 0, 1);
    layout->addWidget(createCellWidget(tr("Ellipse"),
                      QtIconItem::Ellipse), 1, 0);
    layout->addWidget(createCellWidget(tr("Polygon"),
                      QtIconItem::Polygon), 1, 1);

    layout->setRowStretch(3, 10);
    layout->setColumnStretch(2, 10);

    QWidget *itemWidget = new QWidget;
    itemWidget->setLayout(layout);

    toolBox = new QToolBox;
    toolBox->setSizePolicy(QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Ignored));
    toolBox->setMinimumWidth(itemWidget->sizeHint().width());
    toolBox->addItem(itemWidget, tr("Basic Shapes"));
}

void QtIconEditor::createActions()
{
    toFrontAction = new QAction(QIcon(":/images/bringtofront.png"),
                                tr("Bring to &Front"), this);
    toFrontAction->setShortcut(tr("Ctrl+F"));
    toFrontAction->setStatusTip(tr("Bring item to front"));
    connect(toFrontAction, SIGNAL(triggered()),
            this, SLOT(bringToFront()));

    sendBackAction = new QAction(QIcon(":/images/sendtoback.png"),
                                 tr("Send to &Back"), this);
    sendBackAction->setShortcut(tr("Ctrl+B"));
    sendBackAction->setStatusTip(tr("Send item to back"));
    connect(sendBackAction, SIGNAL(triggered()),
        this, SLOT(sendToBack()));

    deleteAction = new QAction(QIcon(":/images/delete.png"),
                               tr("&Delete"), this);
    deleteAction->setShortcut(tr("Delete"));
    deleteAction->setStatusTip(tr("Delete item from diagram"));
    connect(deleteAction, SIGNAL(triggered()),
        this, SLOT(deleteItem()));
}

void QtIconEditor::createToolbars()
{
    editToolBar = addToolBar(tr("Edit"));
    editToolBar->addAction(deleteAction);
    editToolBar->addAction(toFrontAction);
    editToolBar->addAction(sendBackAction);

    fillColorToolButton = new QToolButton;
    fillColorToolButton->setPopupMode(QToolButton::MenuButtonPopup);
    fillColorToolButton->setMenu(createColorMenu(SLOT(itemColorChanged()),
                         Qt::white));
    fillAction = fillColorToolButton->menu()->defaultAction();
    fillColorToolButton->setIcon(createColorToolButtonIcon(
    ":/images/floodfill.png", Qt::white));
    connect(fillColorToolButton, SIGNAL(clicked()),
            this, SLOT(fillButtonTriggered()));

    lineColorToolButton = new QToolButton;
    lineColorToolButton->setPopupMode(QToolButton::MenuButtonPopup);
    lineColorToolButton->setMenu(createColorMenu(SLOT(lineColorChanged()),
                                 Qt::black));
    lineAction = lineColorToolButton->menu()->defaultAction();
    lineColorToolButton->setIcon(createColorToolButtonIcon(
        ":/images/linecolor.png", Qt::black));
    connect(lineColorToolButton, SIGNAL(clicked()),
            this, SLOT(lineButtonTriggered()));


    colorToolBar = addToolBar(tr("Color"));
    colorToolBar->addWidget(fillColorToolButton);
    colorToolBar->addWidget(lineColorToolButton);

    QToolButton *pointerButton = new QToolButton;
    pointerButton->setCheckable(true);
    pointerButton->setChecked(true);
    pointerButton->setIcon(QIcon(":/images/pointer.png"));

    pointerTypeGroup = new QButtonGroup;
    pointerTypeGroup->addButton(pointerButton, int(QtIconScene::MoveItem));
    connect(pointerTypeGroup, SIGNAL(buttonClicked(int)),
            this, SLOT(pointerGroupClicked(int)));

    sceneScaleCombo = new QComboBox;
    QStringList scales;
    scales << tr("50%") << tr("75%") << tr("100%") << tr("125%") << tr("150%");
    sceneScaleCombo->addItems(scales);
    sceneScaleCombo->setCurrentIndex(2);
    connect(sceneScaleCombo, SIGNAL(currentIndexChanged(const QString &)),
            this, SLOT(sceneScaleChanged(const QString &)));

    pointerToolbar = addToolBar(tr("Pointer type"));
    pointerToolbar->addWidget(pointerButton);
    pointerToolbar->addWidget(sceneScaleCombo);
}

QWidget *QtIconEditor::createCellWidget(const QString &text,
                      QtIconItem::DiagramType type)
{

    QtIconItem item(type);
    QIcon icon(item.image());

    QToolButton *button = new QToolButton;
    button->setIcon(icon);
    button->setIconSize(QSize(50, 50));
    button->setCheckable(true);
    buttonGroup->addButton(button, int(type));

    QGridLayout *layout = new QGridLayout;
    layout->addWidget(button, 0, 0, Qt::AlignHCenter);
    layout->addWidget(new QLabel(text), 1, 0, Qt::AlignCenter);

    QWidget *widget = new QWidget;
    widget->setLayout(layout);

    return widget;
}

QMenu *QtIconEditor::createColorMenu(const char *slot, QColor defaultColor)
{
    QList<QColor> colors;
    colors << Qt::black << Qt::white << Qt::red << Qt::blue << Qt::yellow;
    QStringList names;
    names << tr("black") << tr("white") << tr("red") << tr("blue")
          << tr("yellow");

    QMenu *colorMenu = new QMenu;
    for (int i = 0; i < colors.count(); ++i) {
        QAction *action = new QAction(names.at(i), this);
        action->setData(colors.at(i));
        action->setIcon(createColorIcon(colors.at(i)));
        connect(action, SIGNAL(triggered()),
                this, slot);
        colorMenu->addAction(action);
        if (colors.at(i) == defaultColor) {
            colorMenu->setDefaultAction(action);
        }
    }
    return colorMenu;
}

QIcon QtIconEditor::createColorToolButtonIcon(const QString &imageFile,
                        QColor color)
{
    QPixmap pixmap(50, 80);
    pixmap.fill(Qt::transparent);
    QPainter painter(&pixmap);
    QPixmap image(imageFile);
    QRect target(0, 0, 50, 60);
    QRect source(0, 0, 42, 42);
    painter.fillRect(QRect(0, 60, 50, 80), color);
    painter.drawPixmap(target, image, source);

    return QIcon(pixmap);
}

QIcon QtIconEditor::createColorIcon(QColor color)
{
    QPixmap pixmap(20, 20);
    QPainter painter(&pixmap);
    painter.setPen(Qt::NoPen);
    painter.fillRect(QRect(0, 0, 20, 20), color);

    return QIcon(pixmap);
}

void QtIconEditor::createButtonBox()
{
	m_buttonBox = new QDialogButtonBox(this);
    m_buttonBox->setOrientation(Qt::Horizontal);
    m_buttonBox->setStandardButtons(QDialogButtonBox::Apply|QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
    
    QObject::connect(m_buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(clicked(QAbstractButton*)));
}

void QtIconEditor::clicked(QAbstractButton* button)
{
	QDialogButtonBox::StandardButton buttonId = m_buttonBox->standardButton(button);
	if(buttonId == QDialogButtonBox::Apply) {
		this->saveIcon();
		setModified(false);		
	}
	else if(buttonId == QDialogButtonBox::Cancel) {
		this->close();
	}
	else if(buttonId == QDialogButtonBox::Ok) {
		//Ask to save and close
		if(isModified()) {
			int ret = QMessageBox::warning(this, tr("QtDocument"),
	                       tr("The icon : '%1' was modified.\nDo you want to save the changes?").arg(m_iconName),
	                       QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel,
	                       QMessageBox::Cancel);
	        if(ret == QMessageBox::Yes) {
	        	this->saveIcon();
	        	this->close();
	        }
			else if(ret == QMessageBox::No){
				this->close();	
			}
		}
		else {
			this->close();
		}
	}
}

void QtIconEditor::setModified(bool modified)
{
	m_modified = modified;
	QPushButton* buttonApply = m_buttonBox->button(QDialogButtonBox::Apply);
	if(buttonApply) {
		if(m_modified) {
			buttonApply->setEnabled(true);
		}
		else {
			buttonApply->setEnabled(false);
		}
	}
}

bool QtIconEditor::isModified() const
{
	return m_modified;
}

void QtIconEditor::saveIcon()
{
	//set path to save
	QString homePath = QDir::homePath();
	if(!homePath.isEmpty()) {	
		//TODO add directory name in user preference
		QString iconRepository = QString(".flowdesigner") + QDir::separator() + "icons";
		QDir homeDir(homePath);
		if(!homeDir.exists(iconRepository)) {
			homeDir.mkpath(iconRepository);
		}
		QString path = homePath + QDir::separator() + iconRepository + QDir::separator() + m_iconName + ".svg";
		
		//Save image
		QSvgGenerator svgGenerator;
		svgGenerator.setFileName(path);
		svgGenerator.setSize(QSize(30/1.25,30/1.25));
		QPainter painter(&svgGenerator);
		
		if(scene->items().size() != 0) {
			scene->setDrawBackground(false);
			scene->clearSelection();
			scene->render(&painter);
			scene->setDrawBackground(true);
			painter.end();
			emit iconSaved(path);
		}
		else {
			if(QFile::exists(path)) {
				QFile::remove(path);
			}
			emit iconSaved(QString(""));
		}
	}
}
