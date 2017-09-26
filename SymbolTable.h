#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <map>
#include <string>
#include "SymbolTableEntry.h"
using namespace std;

class SYMBOL_TABLE {
private:
  std::map<string, SYMBOL_TABLE_ENTRY> hashTable;

public:
  //Constructor
  SYMBOL_TABLE( ) { }

  // Add SYMBOL_TABLE_ENTRY x to this symbol table.
  // If successful, return true; otherwise, return false.
  bool addEntry(SYMBOL_TABLE_ENTRY x) {
    // Make sure there isn't already an entry with the same name
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(x.getName())) == hashTable.end()) {
      hashTable.insert(make_pair(x.getName(), x));
	return(true);
    }
    else return(false);
  }

  // If a symbol table entry with name theName is
  // found in this symbol table, then set its type and link
  // info in reference params, and return true; otherwise, 
  // return false.
  bool findEntry(string theName, TYPE_INFO &typeInfo,
                 LINKAGE_INFO &linkInfo) 
  {
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(theName)) == hashTable.end())
      return(false);
    else {
          typeInfo = itr->second.getTypeInfo();
          linkInfo = itr->second.getLinkageInfo();
          return(true);
         }
  }

  int frameSize() 
  {
    int cnt = 0;
    for (std::map<string, SYMBOL_TABLE_ENTRY>::iterator
                          it=hashTable.begin();
			          it != hashTable.end(); it++) 
    {
      SYMBOL_TABLE_ENTRY stEntry = 
        SYMBOL_TABLE_ENTRY(it->second);
      switch (stEntry.getTypeInfo().type) 
      {
        case INT		: cnt++; break;
        case CHAR	: cnt++; break;
        case BOOL	: cnt++; break;
        case ARRAY	: cnt = cnt + 
				       (stEntry.getEndIndex() -
					   stEntry.getStartIndex() + 1);
        			  break;
      }
    }
    return(cnt);
  }

};

#endif  // SYMBOL_TABLE_H
