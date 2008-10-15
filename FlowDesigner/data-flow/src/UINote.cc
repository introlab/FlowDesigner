#include "UINote.h"

#include <sstream>
#include <iostream>
#include "UINetwork.h"

//@implements UIClasses

using namespace std;

namespace FD {
	
	UINote::UINote(UINetwork* net, const std::string &label, const std::string &text, double x, double y, bool visible) 
	: m_network(net), m_label(label), m_text(text), m_x(x), m_y(y), m_visible(visible) 
	{
		if (m_network)
		{
			m_network->addNote(this);
		}
	}
	
	UINote::~UINote()
	{
		if (m_network)
		{
			m_network->removeNote(this,false);
		}
	}
	
	void UINote::saveXML(xmlNode *root)
	{
		
		xmlNodePtr tree;
		
		tree = xmlNewChild(root, NULL, (xmlChar *)"Note", NULL);
		
		stringstream x_string;
		x_string << m_x;
		
		stringstream y_string;
		y_string << m_y;
		
		stringstream visible_string;
		visible_string << m_visible;
		
		xmlSetProp(tree, (xmlChar *)"x", (xmlChar *)x_string.str().c_str());
		xmlSetProp(tree, (xmlChar *)"y", (xmlChar *)y_string.str().c_str());
		xmlSetProp(tree, (xmlChar *)"visible",(xmlChar*)visible_string.str().c_str());
		if (!m_text.empty()) {
			xmlSetProp(tree, (xmlChar *)"text", (xmlChar *)m_text.c_str());
		}
		else {
			xmlSetProp(tree, (xmlChar *)"text", (xmlChar*) "Empty!");
		}
		
		if (!m_label.empty()) {
			xmlSetProp(tree, (xmlChar *)"label", (xmlChar *)m_label.c_str());
		} 
		else {
			xmlSetProp(tree, (xmlChar *)"label", (xmlChar *)"No label"); 
		}
	}
	
}//namespace FD

