/***********************************************************************************
** Copyright (C) 2006-2008 Laborius (http://www.gel.usherbrooke.ca/laborius/). 
** All rights reserved. 
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License
** as published by the Free Software Foundation; either version 2
** of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
***********************************************************************************/
#ifndef QTSVGEDITOR_H
#define QTSVGEDITOR_H

#include <QMainWindow>

#include "iconeditor/QtIconItem.h"

class QtIconScene;

QT_BEGIN_NAMESPACE
class QAction;
class QToolBox;
class QSpinBox;
class QComboBox;
class QFontComboBox;
class QButtonGroup;
class QLineEdit;
class QFont;
class QToolButton;
class QAbstractButton;
class QGraphicsView;
class QDialogButtonBox;
QT_END_NAMESPACE

/**
 * An icon editor for QtNode. It provides basic 
 * features to create SVG icon. User icons are 
 * created in the user personal directory under 
 * ".flowdesigner/icons".
 * @author Mathieu Labbe
 */
class QtIconEditor : public QMainWindow
{
    Q_OBJECT;
    
public:
	/**
	 * The graphics scene size.
	 */
	static const int SCENE_SIZE = 500;

public:
   /**
    * The constructor.
    * @param iconName the icon path
    * @param parent the QWidget parent
    */
   QtIconEditor(QWidget* parent, const QString &iconName);

public slots:
	/**
	 * Called when the "Apply", "Cancel" 
	 * or "Ok" buttons are clicked.
	 * @param button the button clicked
	 */
	void clicked(QAbstractButton* button);
	
	/**
	 * Called to bring a specific item to the front.
	 */
	void bringToFront(QGraphicsItem* item);

private slots:
	/**
	 * Called when a button on the toolbox was clicked.
	 * It changes the mode of the scene to the item 
	 * type selected.
	 * @param id the id of the button
	 */
    void buttonGroupClicked(int id);
    
    /**
     * Delete selected item.
     */
    void deleteItem();
    
    /**
     * Called when a button in the pointer group was clicked.
     * It changes the pointer to type selected.
     * @param id the id of the button
     */
    void pointerGroupClicked(int id);
    
    /**
     * Bring selected item to the front.
     */
    void bringToFront();
    
    /**
     * Send selected item to back.
     */
    void sendToBack();
    
    /**
     * Reset the scene mode and unselect the toolbox.
     */
    void itemInserted();
    
    /**
     * Update the view.
     */
    void sceneScaleChanged(const QString &scale);
    
    /**
     * Change color of the fill color button.
     */
    void itemColorChanged();
    
    /**
     * Change color of the line color button.
     */
    void lineColorChanged();
    
    /**
     * Modify fill color of selected item.
     */
    void fillButtonTriggered();
    
    /**
     * Modify line color of selected item.
     */
    void lineButtonTriggered();

signals:
	/**
	 * Emitted when the icon is saved.
	 * @param path the path of the icon file.
	 */
	void iconSaved(QString path);

private:
	/**
	 * Create and setup the tool box.
	 */
    void createToolBox();
    
    /**
     * Create and setup the actions.
     */
    void createActions();
    
    /**
     * Create and setup the menus.
     */
    void createMenus();
    
    /**
     * Create and setup the toolbars.
     */
    void createToolbars();
    
    /**
     * Create and setup the button box.
     */
    void createButtonBox();
    
    /**
     * Create widget for a button in the toolbox.
     * @param text the text to add to the widget
     * @param type the diagram item type
     * @return QWidget the widget created
     */
    QWidget *createCellWidget(const QString &text,
                              QtIconItem::DiagramType type);
    
    /**
     * Create the color menu.
     * @param slot a slot used when the signal triggered() 
     * of an action of the menu is emitted.
     * @param defaultColor the default color
     * @return the color menu
     */
    QMenu *createColorMenu(const char *slot, QColor defaultColor);
    
    /**
     * Create a color icon with image for button.
     * @param image the image of the icon
     * @param color the icon color
     * @return QIcon the icon created
     */
    QIcon createColorToolButtonIcon(const QString &image, QColor color);
    
    /**
     * Create a color icon.
     * @param color the color of the icon
     * @return icon the icon created
     */
    QIcon createColorIcon(QColor color);
    
    /**
     * Save the icon.
     */
    void saveIcon();
    
    /**
     * Setter.
     * @param modified true or false
     */
    void setModified(bool modified);
    
    /**
     * Getter.
     * @return true if modified, otherwise false
     */
    bool isModified() const;
   
private:
    QtIconScene *scene;
    QGraphicsView *view;

    QAction *addAction;
    QAction *deleteAction;

    QAction *toFrontAction;
    QAction *sendBackAction;
    QAction *aboutAction;

    QToolBar *editToolBar;
    QToolBar *colorToolBar;
    QToolBar *pointerToolbar;

    QComboBox *sceneScaleCombo;
    QComboBox *itemColorCombo;

    QToolBox *toolBox;
    QButtonGroup *buttonGroup;
    QButtonGroup *pointerTypeGroup;
    QToolButton *fillColorToolButton;
    QToolButton *lineColorToolButton;
    QAction *fillAction;
    QAction *lineAction;
    
    QDialogButtonBox* m_buttonBox;
    
    QString m_iconName;
    
    bool m_modified;
};

#endif
