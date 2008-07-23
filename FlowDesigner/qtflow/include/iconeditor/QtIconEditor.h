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

//! [0]
class QtIconEditor : public QMainWindow
{
    Q_OBJECT;

public:
   QtIconEditor(const QString &iconName, QWidget* parent);
   static const int SCENE_SIZE = 500;

public slots:
	void clicked(QAbstractButton* button);
	void bringToFront(QGraphicsItem* item);

private slots:
    void buttonGroupClicked(int id);
    void deleteItem();
    void pointerGroupClicked(int id);
    void bringToFront();
    void sendToBack();
    void itemInserted();
    void sceneScaleChanged(const QString &scale);
    void itemColorChanged();
    void lineColorChanged();
    void fillButtonTriggered();
    void lineButtonTriggered();

signals:
	void iconSaved(QString path);

private:
    void createToolBox();
    void createActions();
    void createMenus();
    void createToolbars();
    void createButtonBox();
    QWidget *createCellWidget(const QString &text,
                              QtIconItem::DiagramType type);
    QMenu *createColorMenu(const char *slot, QColor defaultColor);
    QIcon createColorToolButtonIcon(const QString &image, QColor color);
    QIcon createColorIcon(QColor color);
    
    void saveIcon();
    void setModified(bool modified);
    bool isModified() const;

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
