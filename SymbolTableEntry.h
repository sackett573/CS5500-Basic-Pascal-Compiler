#ifndef SYMBOL_TABLE_ENTRY_H
#define SYMBOL_TABLE_ENTRY_H

#include <string>
using namespace std;

#define UNDEFINED  			-1
#define PROCEDURE			0
#define INT				1
#define CHAR				2
#define INT_OR_CHAR			3
#define BOOL				4
#define INT_OR_BOOL			5
#define CHAR_OR_BOOL			6
#define INT_OR_CHAR_OR_BOOL		7
#define ARRAY				8
#define INDEX_RANGE			9
#define PROGRAM				10

#define NOT_APPLICABLE 		-1

typedef struct { 
  int type;
  int startIndex;
  int endIndex;
  int baseType;
} TYPE_INFO;

typedef struct {
  int offset;
  int level;
  int labelNum;
  int frameSize;
} LINKAGE_INFO;

class SYMBOL_TABLE_ENTRY 
{
private:
  // Member variables
  string name;
  TYPE_INFO typeInfo;
  LINKAGE_INFO linkage;  

public:
  // Constructors
  SYMBOL_TABLE_ENTRY( ) { 
    name = ""; 
    typeInfo.type = UNDEFINED;                       
    typeInfo.startIndex = UNDEFINED; 			    
    typeInfo.endIndex = UNDEFINED; 				    
    typeInfo.baseType = UNDEFINED;
    linkage.offset = UNDEFINED;
    linkage.level = UNDEFINED;
    linkage.labelNum = UNDEFINED;
    linkage.frameSize = UNDEFINED;
  }

  SYMBOL_TABLE_ENTRY(const string theName, const int theType,
                     const int theStart, const int theEnd,
                     const int theBaseType,
			     const int theOffset, const int theLevel,
			     const int theLabel, const int theFrame) 
  {
    name = theName;
    typeInfo.type = theType;
    typeInfo.startIndex = theStart;
    typeInfo.endIndex = theEnd;
    typeInfo.baseType = theBaseType;
    linkage.offset = theOffset;
    linkage.level = theLevel;
    linkage.labelNum = theLabel;
    linkage.frameSize = theFrame;
  }

  SYMBOL_TABLE_ENTRY(const string theName, 
                     const TYPE_INFO info,
			     const LINKAGE_INFO link) 
  {
    name = theName;
    typeInfo.type = info.type;
    typeInfo.startIndex = info.startIndex;
    typeInfo.endIndex = info.endIndex;
    typeInfo.baseType = info.baseType;
    linkage.offset = link.offset;
    linkage.level = link.level;
    linkage.labelNum = link.labelNum ;
    linkage.frameSize = link.frameSize;
  }

  // Accessors
  string getName() const { return name; }
  TYPE_INFO getTypeInfo() const { return typeInfo; }
  int getTypeCode() const { return typeInfo.type; }
  int getStartIndex() const { return typeInfo.startIndex; }
  int getEndIndex() const { return typeInfo.endIndex; }
  int getBaseType() const { return typeInfo.baseType; }
  LINKAGE_INFO getLinkageInfo() const { return linkage; }
};

#endif  // SYMBOL_TABLE_ENTRY_H
