// Copyright (C) 2001 Dominic Letourneau (doumdi@yahoo.com)

#include <strstream> //used for strstream
#include "Tokenizer.h"



const static string FILE_NAME = __FILE__;

void string_to_token(vector<string> &outputVector, const string &inputString, vector<char> &keepToken, vector<char> &discardToken)
{
  //Clear the output 
  outputVector.clear();

  //Place the string in a vector to manipulate it
  vector<char> tempVector(inputString.begin(), inputString.end());

  //Declare and initialize tempVector iterator
  vector<char>::iterator charVectorItBegin(tempVector.begin()), charVectorItEnd(tempVector.begin());

  //Test that the keepToken and discardToken vector don't contain an identical element
  for (vector<char>::iterator tempTokenIt(keepToken.begin()); tempTokenIt < keepToken.end(); tempTokenIt++) {
    if (discardToken.end() != find(discardToken.begin(), discardToken.end(), *tempTokenIt)) {
 
      //      cerr << "ERROR in string_to_token keepToken and discardToken contain an identical character" << endl << flush;
      charVectorItBegin = tempVector.end();
      charVectorItEnd = tempVector.end();
    }
  }

  while ((charVectorItBegin < tempVector.end()) && (charVectorItEnd < tempVector.end())) {

    //Remove initial white space
    for (; ((0 != isspace(*charVectorItBegin)) && (charVectorItBegin < tempVector.end())); 
      charVectorItBegin++) {/* DO NOTHING */}

    //Verify if we have reached the end of the string
    if (charVectorItBegin >= tempVector.end()) {
      break;
    }

    //Verify if the current character is a discardable character
    if (discardToken.end() != find(discardToken.begin(), discardToken.end(), *charVectorItBegin)) {
      //Token is to be discarded so increment the begin iterator and continue
      charVectorItBegin++;
      continue;
    }

    //Verify if the current character is single character token that we want to keep
    if (keepToken.end() != find(keepToken.begin(), keepToken.end(), *charVectorItBegin)) {
      //Put the single character token in the outputVector, increment the begin iterator and continue
      outputVector.push_back(string(1,*charVectorItBegin));
      charVectorItBegin++;
      continue;
    }

    //Initialize the end iterator and
    //Move the end iterator to the end of the token being formed.
    //Check for the end of the vector
    for (charVectorItEnd = charVectorItBegin + 1 ; charVectorItEnd < tempVector.end() ; charVectorItEnd++) {
      //Check for a space character
      if (0 != isspace(*charVectorItEnd)) {
        break;
      }
      //Check for a discardable character
      if (discardToken.end() != find(discardToken.begin(), discardToken.end(), *charVectorItEnd)) {
        break;
      }
      //Check for a single character token
      if (keepToken.end() != find(keepToken.begin(), keepToken.end(), *charVectorItEnd)) {
        break;
      }
    }

    //Check for iterator error
    if (charVectorItBegin == charVectorItEnd) {

      //      cerr << "ERROR in string_to_token begin and end iterator are identical!" << endl << flush;
      break;
    }
    if (charVectorItBegin == tempVector.end()) {

      //      cerr << "ERROR in string_to_token begin iterator out of bound!" << endl << flush;
      break;
    }

    //Iterator is valid, place token in the output vector
    outputVector.push_back(string(charVectorItBegin, charVectorItEnd));

    //Reset the begin iterator
    charVectorItBegin = charVectorItEnd;

  }

  return;
}


bool string_contain_nl_sys_command(vector<string> &outputVector, string &inputString)
{
  //Clear the output 
  outputVector.clear();

  vector<char> keepToken, discardToken;
  keepToken.clear();
  discardToken.clear();

  string_to_token(outputVector, inputString, keepToken, discardToken);

  if ("nl_sys_command" == *(outputVector.begin())) {
    return true;
  }

  return false;
}


string vector_of_string_to_string(vector<string>::iterator &startOfStringIt, vector<string> &inputVector, string stringSeparator = " ")
{
  string returnString = "";

  if (startOfStringIt < inputVector.end()) {
    returnString = *startOfStringIt;
  } else {
    return returnString;
  } 

  for (startOfStringIt++; startOfStringIt != inputVector.end(); startOfStringIt++) {
    returnString += stringSeparator;
    returnString += *startOfStringIt;
  }

  return returnString;
}


string int_to_string(int inputInteger)
{
  string tempString;

  //a complex way of transforming an int into a string...
  strstream tempBuf;
  tempBuf << inputInteger << ends; //you must append ends to the constructed string as the null character is not otherwise added to the string. ???
  char *pcTemp = tempBuf.str();
  tempString = pcTemp;
  delete pcTemp;

  return tempString;
}
