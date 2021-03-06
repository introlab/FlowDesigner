// Copyright (C) 2001 Luc Lussier

#include <iterator>
#include <vector>
#include <fstream>
#include <string>
#include <algorithm>
#include "Vector.h"


#ifndef NL_SYS_STRING_TO_TOKEN_H
#define NL_SYS_STRING_TO_TOKEN_H

namespace FD {

//extern vector<char> globalKeepTokenVector;
//Should be moved to a less module specific location

/** Global function.
Transform a string type string to a vector of string token. Each token is separated either by white 
space or by specified discardToken. Special keepToken can also be specified individually. See the 
test file for a demonstration of the behavior.

@param outputVector A vector<string> to which the tokens are added
@param inputString A string that will be cut into tokens
@param keepToken A vector of character that specify some character that should be made into a single character token
@param discardToken A vector of character that are to be used like white spaces to break tokens
@author Luc Lussier
*/
void string_to_token(Vector<std::string> &outputVector, const std::string &inputString, Vector<char> &keepToken, Vector<char> &discardToken);



/** Global function.
This function look for a "nl_sys_command" token at the first position of the string to determine if 
the string contain a command.
Commands are used by the various modules to communicate between each others. Each module define it's own 
commands which can contain an arbitrary number of parameters.

@param outputVector A vector<string> to which string tokens are added.
@param inputString A string that will be cut into tokens.
@return True if the string contain a command and False if no command is found inside the string.
@author Luc Lussier
*/
bool string_contain_nl_sys_command(Vector<std::string> &outputVector, std::string &inputString);



/** Global function.
This function is used to transform a vector<string> back into a string. The vector token are separated by the specified 
string separator or by default by a white space.

@param startOfStringIt Iterator pointing to the first element to add to the string.
@param inputVector A vector<string> that will be transformed back into a string.
@param cSeparator Default parameter. The string added between the string token as a separator while forming the new string. The default is " ".
@return A string formed from the inputVector
@author Luc Lussier
*/
std::string vector_of_string_to_string(Vector<std::string>::iterator &startOfStringIt, Vector<std::string> &inputVector, std::string stringSeparator = " ");

/** Global function.
This function is used to transform an integer into a string.

@param inputInteger The integer to transform into a string.
@return a string.
@author Luc Lussier
*/
std::string int_to_string(int inputInteger);

}//namespace FD
#endif
