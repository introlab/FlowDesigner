#ifndef _UINOTE_H_
#define _UINOTE_H_

#include <string>
#include <vector>
#include <libxml/tree.h>

//Copyright (C) Dominic Letourneau 2004
//

/**
   UINote is a "Post-It" like note incorporated into a network for documentation purposes.
   It is not useful for calculation and is not connected anywhere.
*/
class UINote {

 protected:
  
  ///Text of the note
  std::string m_text;  
  ///X position on the canvas
  double m_x;
  ///Y position on the canvas
  double m_y;
  ///Is the Note visible ?  
  bool m_visible;
 public:
  
  /**
    UINote Constructor
    \param test The text of the note
    \param x the x position of the note on the canvas
    \param y the y position of the note on the canvas
  */
  UINote(const std::string &text, double x, double y, bool visible = true);
  
  ///UINote destructor
  virtual ~UINote();
  
  /**
     Save UINote in the XML format
     \param root The XML root where to save
  */
  void saveXML(xmlNode *root);
  
  /**
     Get the text content of the note
     \return std::string the text contained in the UINote
  */
  std::string getText() {return m_text;}
  
  /**
     Set the text of the note
     \param text the text to be contained in the UINote
  */
  void setText(const std::string &text){m_text =text;}
   
  /**
     \return bool True if the note is visible, else false
  */
  bool isVisible() {return m_visible;}
  
  /**
     Set the visibility of the note
     \param visible True if the note should be visible, else false
  */
  void setVisible(bool visible) {m_visible = visible;}

};
#endif
