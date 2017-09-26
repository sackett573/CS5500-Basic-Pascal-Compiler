%{

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <ctype.h>
#include <stack>
#include <list>
#include "SymbolTable.h"
using namespace std;

void ignoreComment();
int ckInt();
int curVal, curVal2;
int readOffset, readLevel;
int assignOffset, assignLevel;
bool inAdd = false;
bool isVar = false;
bool isAssign = false;
void prRule(const char*, const char*);
void printTokenInfo(const char* tokenType, 
                    const char* lexeme);
void prSymbolTableAddition(const string identName, 
                           const TYPE_INFO typeInfo);
void prLabel(const int lnum);
void prAsp(const int offset);
void prBss(const int s);
void prComment(const char *s, const int offset);
void prDeref();
void prEnd();
void prHalt();
void prInit(const int g, const int d, const int s, 
            const int c, const int e);
void prLA(const int offset, const int level);
void prLC(const int val);
void prRead(const int varType);
void prSave(const int level);
void prST();
void prWrite(const int varType);
void prJF(const int lab);
void prJP(const int lab);
void prJI();
void prJS(const int lab);
void prOp(const int op);
void prPush(const int offset, const int level);
void prPop(const int offset, const int level);

void beginScope();
void endScope();
void cleanUp();
bool findEntryInAnyScope(const string theName,
                         TYPE_INFO &typeInfo,
				   LINKAGE_INFO &linkInfo);

int yyerror(const char*);

extern "C" {
    int yyparse(void);
    int yylex(void);
    int yywrap() {return 1;}
}

#define MAX_INT	"2147483647"

#define OUTPUT_TOKENS	     0
#define OUTPUT_PRODUCTIONS 0
#define OUTPUT_ST_MGT      0
#define GENERATING_CODE    1

#define LOGICAL_OP    100
#define ARITHMETIC_OP 101

#define POSITIVE		1
#define NEGATIVE		-1
#define NO_SIGN		0

int lineNum = 1;

stack<SYMBOL_TABLE> scopeStack;
list<string> variableNames;

list<int> procedureLabels;
list<int> elseLabels;

int labelNum = 4;
int level = 0;
int offset = 0;

bool global_declarations = true;

%}

%union {
  char* text;
  char ch;
  int num;
  bool boolean;
  TYPE_INFO typeInfo;
};

%token      T_LPAREN    T_RPAREN    T_MULT	    T_PLUS
%token      T_COMMA     T_MINUS     T_DOT       T_DOTDOT
%token      T_COLON     T_ASSIGN    T_SCOLON    T_LT
%token      T_LE        T_NE        T_EQ        T_GT
%token      T_GE        T_LBRACK    T_RBRACK    T_DO
%token      T_AND       T_ARRAY     T_BEGIN     T_BOOL
%token      T_CHAR      T_CHARCONST T_DIV 	     T_END       
%token      T_FALSE     T_IDENT	    T_IF        T_INT
%token 	 T_INTCONST  T_NEG
%token      T_NOT       T_OF        T_OR        T_PROC
%token      T_PROG      T_READ      T_TRUE      
%token      T_VAR       T_WHILE     T_WRITE     T_UNKNOWN

%token      ST_EOF

%type <ch> T_CHARCONST
%type <num> N_IDX T_INTCONST N_ADDOP N_SIGN N_INTCONST
%type <num> N_RELOP N_ADDOP_ARITH N_ADDOP_LOGICAL T_IF
%type <num> N_MULTOP N_MULTOP_ARITH N_MULTOP_LOGICAL T_WHILE
%type <num> N_COND_IF_THEN  
%type <text> T_IDENT N_IDENT
%type <typeInfo> N_ARRAY N_BOOLCONST N_CONST  
%type <typeInfo> N_ENTIREVAR N_ARRAYVAR N_INPUTVAR
%type <typeInfo> N_VARIDENT N_FACTOR N_TERM N_VARIABLE 
%type <typeInfo> N_IDXRANGE N_EXPR N_SIMPLE N_SIMPLEEXPR 
%type <typeInfo> N_PROCIDENT N_IDXVAR N_TYPE N_SUB_RANGE

%nonassoc   T_THEN
%nonassoc   T_ELSE

%start      N_START

%%
N_START         :   {
			    prInit(0,20,1,2,3);
			    prLabel(0);
			    }
			  N_PROG
                    {
			    prRule("N_START", "N_PROG");
			    if (!GENERATING_CODE)
			     printf("\n---- Completed parsing ----\n\n");
			    prHalt();
			    prLabel(1);
			    prBss(500);
			    prEnd();
			    return 0;
                    }
                ;
N_ADDOP         : N_ADDOP_LOGICAL
                    {
                    prRule("N_ADDOP", "N_ADDOP_LOGICAL");
			    $$ = $1;
                    }
                | N_ADDOP_ARITH
                    {
                    inAdd = true;
										prRule("N_ADDOP", "N_ADDOP_ARITH");
			    $$ = $1;
                    }
                ;
N_ADDOP_LOGICAL : T_OR
                    {
                    prRule("N_ADDOP_LOGICAL", "T_OR");
			    $$ = T_OR;
                    }
                ;
N_ADDOP_ARITH   : T_PLUS
                    {
                    prRule("N_ADDOP_ARITH", "T_PLUS");
			    $$ = T_PLUS;
                    }
                | T_MINUS
                    {
                    prRule("N_ADDOP_ARITH", "T_MINUS");
			    $$ = T_MINUS;
                    }
                ;
N_ADDOPLST      : /* epsilon */
                    {
                    prRule("N_ADDOPLST", "epsilon");
                    }
                | N_ADDOP N_TERM N_ADDOPLST
                    {
                    prRule("N_ADDOPLST", 
                           "N_ADDOP N_TERM N_ADDOPLST");
			    if (($1 == T_OR) && 
			        ($2.type != BOOL)) {
			      yyerror("Expression must be of type boolean");
			      return(0);
			    }
			    else if (($1 != T_OR) &&
				        ($2.type != INT)) {
			      yyerror("Expression must be of type integer");
			      return(0);
			    }
			    // Print operator postfix
			    prOp($1);
					             inAdd = false;
                    }
                ;
N_ARRAY         : T_ARRAY T_LBRACK N_IDXRANGE T_RBRACK T_OF
			  N_SIMPLE
                    {
                    	prRule("N_ARRAY",
                        "T_ARRAY T_LBRACK N_IDXRANGE T_RBRACK T_OF N_SIMPLE");
				$$.type = ARRAY; 
                      $$.startIndex = $3.startIndex;
                      $$.endIndex = $3.endIndex;
		        	$$.baseType = $6.type;
                    }
                ;
N_ARRAYVAR      : N_ENTIREVAR
                    {
                    prRule("N_ARRAYVAR", "N_ENTIREVAR");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
			    if ($1.type != ARRAY) {
                      yyerror("Indexed variable must be of array type");
                      return(0);
                     }
                    }
                ;
N_ASSIGN        : N_VARIABLE T_ASSIGN N_EXPR
                    {
                    prRule("N_ASSIGN", 
                           "N_VARIABLE T_ASSIGN N_EXPR");
			    if ($1.type == ARRAY) {
                      yyerror("Cannot make assignment to an array");
                      return(0);
                     }
			    if ($1.type == PROCEDURE) {
			      yyerror("Procedure/variable mismatch");
                      return(0);
			    }
			    if ($3.type != $1.type) {
                      yyerror("Expression must be of same type as variable");
                      return(0);
                     }
					prST();
					if($1.startIndex != NOT_APPLICABLE)
					{
						if(curVal >= $1.startIndex && curVal <= $1.endIndex && isVar == false)
						{}
						else if(isVar == true)
						{
						  cout << "  la " << assignOffset << ", " << assignLevel << "\n";
							cout << "  deref\n";
							cout << "  lc " << $1.startIndex << "\n";
							cout << "  .ge.\n";
							cout << "  jf L." << labelNum << "\n";
							labelNum = labelNum + 2;
							cout << "  la " << assignOffset << ", " << assignLevel << "\n";
							cout << "  deref\n";
							cout << "  lc " << $1.endIndex << "\n";
							cout << "  .le.\n";
							cout << "  jf L." << labelNum << "\n";
							labelNum = labelNum + 1;
							cout << "  jp L." << labelNum - 2 << "\n";
							cout << "L." << labelNum - 1 << ":\n";
							cout << "  lc 83\n";
							cout << "  cwrite\n";
							cout << "  lc 82\n";
							cout << "  cwrite\n";
							cout << "  lc 69\n";
							cout << "  cwrite\n";
							cout << "  jp L.1\n";
							cout << "L." << labelNum - 3 << ":\n";
							cout << "  lc 83\n";
							cout << "  cwrite\n";
							cout << "  lc 82\n";
							cout << "  cwrite\n";
							cout << "  lc 69\n";
							cout << "  cwrite\n";
							cout << "  jp L.1\n";
							cout << "L." << labelNum - 2 << ":\n";
						}
						else
						{
						  yyerror("Assigned value is outside of Subrange");
						}
					}
					curVal = 0;
					isVar = false;
					isAssign = false;
                    }
                ;
N_BLOCK         : N_VARDECPART
                    {
                    prRule("N_BLOCK", 
                        "N_VARDECPART N_PROCDECPART N_STMTPART");
	    		    if (global_declarations) {
			      // bss 20 + size of globals
			      prBss(20+scopeStack.top().frameSize());
			      prLabel(2);
			      global_declarations = false;
			    }
                    }
			  N_PROCDECPART
			    {
			    if (!procedureLabels.empty()) {
			      prLabel(procedureLabels.front());
			      procedureLabels.pop_front();
			      prSave(level);
			      prAsp(scopeStack.top().frameSize());
			    }
			    if ((level == 0) && 
			         procedureLabels.empty())
			      prLabel(3); // STMTPART beginning "main" 
			    printf("# Beginning of block's N_STMTPART\n");
			    }
 			  N_STMTPART
			    {
			    }
                ;
N_BOOLCONST     : T_TRUE
                    {
                    prRule("N_BOOLCONST", "T_TRUE");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
			    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
			    prLC(1);
                    }
                | T_FALSE
                    {
                    prRule("N_BOOLCONST", "T_FALSE");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
			    prLC(0);
                    }
                ;
N_COMPOUND      : T_BEGIN N_STMT N_STMTLST T_END
                    {
                    prRule("N_COMPOUND", 
                           "T_BEGIN N_STMT N_STMTLST T_END");
                    }
                ;
N_COND_IF_THEN  : T_IF N_EXPR
			    {
			    if ($2.type != BOOL) {
			      yyerror("Expression must be of type boolean");
			      return(0);
			    }
			     $$ = labelNum;
			     prJF(labelNum);
			     // reserve another label to go after
			     // the "else" stmt (if there isn't
			     // an "else", this label will go
			     // after the "then" stmt)
			     elseLabels.push_front($$+1);
			     labelNum = labelNum + 2; 
			    }
			;
N_CONDITION     : N_COND_IF_THEN T_THEN N_THEN_STMT 
                    {
                    prRule("N_CONDITION", 
                           "T_IF N_EXPR T_THEN N_STMT");
			    prLabel($1+1);
			    }
                |  
			  N_COND_IF_THEN T_THEN N_THEN_STMT T_ELSE N_STMT 
                    {
                    prRule("N_CONDITION",
                      "T_IF N_EXPR T_THEN N_STMT T_ELSE N_STMT");
 			    prLabel($1+1); 
			    }
                ;
N_THEN_STMT	: N_STMT
			  {
			   int elseLabel = elseLabels.front();
			   elseLabels.pop_front();
			   prJP(elseLabel);
			   prLabel(elseLabel-1); 
			  }
			;
N_CONST         : N_INTCONST
                    {
                    prRule("N_CONST", "N_INTCONST");
			    $$.type = INT; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
			    prLC($1);
					if(inAdd == true)
					{
						curVal += $1;
					}
					else
					{
					curVal = $1;
				  }
                    }
                | T_CHARCONST
                    {
                    prRule("N_CONST", "T_CHARCONST");
			    $$.type = CHAR; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
			    prLC($1);
					if(inAdd == true)
					{
						curVal += $1;
					}
					else
					{
					curVal = $1;
				  }
                    }
                | N_BOOLCONST
                    {
                    prRule("N_CONST", "N_BOOLCONST");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_ENTIREVAR     : N_VARIDENT
                    {
                    prRule("N_ENTIREVAR", "N_VARIDENT");
                    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_EXPR          : N_SIMPLEEXPR
                    {
                    prRule("N_EXPR", "N_SIMPLEEXPR");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                | N_SIMPLEEXPR N_RELOP N_SIMPLEEXPR
                    {
                    prRule("N_EXPR", 
                           "N_SIMPLEEXPR N_RELOP N_SIMPLEEXPR");
			    if ($1.type != $3.type) {
			      yyerror("Expressions must both be int, or both char, or both boolean");
			      return(0);
			    }
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
			    // Print operator postfix
			    prOp($2);
                    }
                ;
N_FACTOR        : N_SIGN N_VARIABLE
                    {
                    isVar = true;
										prRule("N_FACTOR", "N_SIGN N_VARIABLE");
			    if (($1 != NO_SIGN) && ($2.type != INT)) {
			      yyerror("Expression must be of type integer");
			      return(0);
			    }
      		    $$.type = $2.type; 
                    $$.startIndex = $2.startIndex;
                    $$.endIndex = $2.endIndex;
		         $$.baseType = $2.baseType;
			    prDeref();
			    if ($1 == NEGATIVE)
			      prOp(T_NEG);
                    }
                | N_CONST
                    {
                    prRule("N_FACTOR", "N_CONST");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                | T_LPAREN N_EXPR T_RPAREN
                    {
                    prRule("N_FACTOR", 
                          "T_LPAREN N_EXPR T_RPAREN");
			    $$.type = $2.type; 
                    $$.startIndex = $2.startIndex;
                    $$.endIndex = $2.endIndex;
		         $$.baseType = $2.baseType;
                    }
                | T_NOT N_FACTOR
                    {
                    prRule("N_FACTOR", "T_NOT N_FACTOR");
			    if ($2.type != BOOL) {
			      yyerror("Expression must be of type boolean");
			      return(0);
			    }
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
			    // Print 'not' operator postfix
			    prOp(T_NOT);
                    }
                ;
N_IDENT         : T_IDENT
                    {
                    prRule("N_IDENT", "T_IDENT");
                    $$ = $1;
                    }
                ;
N_IDENTLST      : /* epsilon */
                    {
                    prRule("N_IDENTLST", "epsilon");
                    }
                | T_COMMA N_IDENT N_IDENTLST
                    {
                    prRule("N_IDENTLST", 
                           "T_COMMA N_IDENT N_IDENTLST");
			    string varName = string($2);
			    variableNames.push_front(varName);
                    }
                ;
N_IDX           : N_INTCONST
                    {
                    prRule("N_IDX", "N_INTCONST");
                    $$ = $1;
                    }
                ;
N_IDXRANGE      : N_IDX T_DOTDOT N_IDX
                    {
                    prRule("N_IDXRANGE", "N_IDX T_DOTDOT N_IDX");
		         $$.type = INDEX_RANGE; 
                    $$.startIndex = $1;
                    $$.endIndex = $3;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_IDXVAR        : N_ARRAYVAR T_LBRACK N_EXPR T_RBRACK
                    {
                    prRule("N_IDXVAR", 
                          "N_ARRAYVAR T_LBRACK N_EXPR T_RBRACK");
	    		   if ($3.type != INT) {
               yyerror("Index expression must be of type integer");
                      return(0);
                     }
			   $$.type = $1.baseType; 
			   $$.startIndex = NOT_APPLICABLE;
			   $$.endIndex = NOT_APPLICABLE;
			   $$.baseType = NOT_APPLICABLE;
			   prOp(T_PLUS);
                    }
                ;
N_INPUTLST      : /* epsilon */
                    {
                    prRule("N_INPUTLST", "epsilon");
                    }
                | T_COMMA N_INPUTVAR N_INPUTLST
                    {
                    prRule("N_INPUTLST", 
                           "T_COMMA N_INPUTVAR N_INPUTLST");
                    }
                ;
N_INPUTVAR      : N_VARIABLE
                    {
                    prRule("N_INPUTVAR", "N_VARIABLE");
			    if (($1.type != INT) && ($1.type != CHAR)) {
              yyerror("Input variable must be of type integer or char");
                      return(0);
                    }
		         $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
			    prRead($1.type);
					          if($$.startIndex != NOT_APPLICABLE)
										{
										  cout << "  la " << readOffset << ", " << readLevel << "\n";
											cout << "  deref\n";
											cout << "  lc " << $$.startIndex << "\n";
											cout << "  .ge.\n";
											cout << "  jf L." << labelNum << "\n";
											labelNum = labelNum + 2;
											cout << "  la " << readOffset << ", " << readLevel << "\n";
											cout << "  deref\n";
											cout << "  lc " << $$.endIndex << "\n";
											cout << "  .le.\n";
											cout << "  jf L." << labelNum << "\n";
											labelNum = labelNum + 1;
											cout << "  jp L." << labelNum - 2 << "\n";
											cout << "L." << labelNum - 1 << ":\n";
											cout << "  lc 83\n";
											cout << "  cwrite\n";
											cout << "  lc 82\n";
											cout << "  cwrite\n";
											cout << "  lc 69\n";
											cout << "  cwrite\n";
											cout << "  jp L.1\n";
											cout << "L." << labelNum - 3 << ":\n";
											cout << "  lc 83\n";
											cout << "  cwrite\n";
											cout << "  lc 82\n";
											cout << "  cwrite\n";
											cout << "  lc 69\n";
											cout << "  cwrite\n";
											cout << "  jp L.1\n";
											cout << "L." << labelNum - 2 << ":\n";
										}
                    }
                ;
N_INTCONST      : N_SIGN T_INTCONST
                    {
                    prRule("N_INTCONST", "N_SIGN T_INTCONST");
			    if ($1 == NO_SIGN)
			      $$ = $2;
			    else $$ = $1 * $2;
                    }
                ;
N_MULTOP        : N_MULTOP_LOGICAL
                    {
                    prRule("N_MULTOP", "N_MULTOP_LOGICAL");
			    $$ = $1;
                    }
                | N_MULTOP_ARITH
                    {
                    prRule("N_MULTOP", "N_MULTOP_ARITH");
			    $$ = $1;
                    }
                ;
N_MULTOP_LOGICAL : T_AND
                    {
                    prRule("N_MULTOP_LOGICAL", "T_AND");
			    $$ = T_AND;
                    }
                ;
N_MULTOP_ARITH  : T_MULT
                    {
                    prRule("N_MULTOP_ARITH", "T_MULT");
			    $$ = T_MULT;
                    }
                | T_DIV
                    {
                    prRule("N_MULTOP_ARITH", "T_DIV");
			    $$ = T_DIV;
                    }
                ;
N_MULTOPLST     : /* epsilon */
                    {
                    prRule("N_MULTOPLST", "epsilon");
                    }
                | N_MULTOP N_FACTOR N_MULTOPLST
                    {
                    prRule("N_MULTOPLST", 
                           "N_MULTOP N_FACTOR N_MULTOPLST");
			    if (($1 == T_AND) && 
			        ($2.type != BOOL)) {
			      yyerror("Expression must be of type boolean");
			      return(0);
			    }
			    else if (($1 != T_AND) &&
				        ($2.type != INT)) {
			      yyerror("Expression must be of type integer");
			      return(0);
			    }
			    // Print operator postfix
			    prOp($1);
                    }
                ;
N_OUTPUT        : N_EXPR
                    {
                    prRule("N_OUTPUT", "N_EXPR");
			    if (($1.type != INT) && ($1.type != CHAR)) {
              yyerror("Output expression must be of type integer or char");
                      return(0);
                    }
			    prWrite($1.type);
                    }
                ;
N_OUTPUTLST     : /* epsilon */
                    {
                    prRule("N_OUTPUTLST", "epsilon");
                    }
                | T_COMMA N_OUTPUT N_OUTPUTLST
                    {
                    prRule("N_OUTPUTLST", "T_COMMA N_OUTPUT N_OUTPUTLST");
                    }
                ;
N_PROCDEC       : N_PROCHDR 
                    {
                    prRule("N_PROCDEC", "N_PROCHDR N_BLOCK");
			    }
			  N_BLOCK
			    {
			    prAsp(-scopeStack.top().frameSize());
			    prJI();
			    endScope();
			    level--;
                    }
                ;
N_PROCHDR       : T_PROC T_IDENT T_SCOLON
                    {
                    prRule("N_PROCHDR", 
                           "T_PROC T_IDENT T_SCOLON");
			    string lexeme = string($2);
			    // Note: don't know proc's framesize yet!
			    TYPE_INFO info = {PROCEDURE, NOT_APPLICABLE,
				           NOT_APPLICABLE, NOT_APPLICABLE};
			    LINKAGE_INFO link = {NOT_APPLICABLE, level+1,
						 labelNum, 0};
			    prSymbolTableAddition(lexeme, info);
                    bool success = scopeStack.top().addEntry
                               (SYMBOL_TABLE_ENTRY(lexeme,info, link));
                	    if (! success) {
                      yyerror("Multiply defined identifier");
                      return(0);
                    }
			    procedureLabels.push_front(labelNum);
			    labelNum++;
			    beginScope();
			    offset = 0;
			    level++;
                    }
                ;
N_PROCDECPART   : /* epsilon */
                    {
                    prRule("N_PROCDECPART", "epsilon");
                    }
                | N_PROCDEC T_SCOLON N_PROCDECPART
                    {
                    prRule("N_PROCDECPART",
                           "N_PROCDEC T_SCOLON N_PROCDECPART");
                    }
                ;
N_PROCIDENT     : T_IDENT
                    {
                    prRule("N_PROCIDENT", "T_IDENT");
			    string ident = string($1);
                	    TYPE_INFO typeInfo;
			    LINKAGE_INFO linkInfo;
                	    if (!findEntryInAnyScope(ident, 
                                          typeInfo, linkInfo)) {
                	      yyerror("Undefined identifier");
                	      return(0);
               	    }
			    $$.type = typeInfo.type;
			    $$.startIndex = typeInfo.startIndex;
			    $$.endIndex = typeInfo.endIndex;
			    $$.baseType = typeInfo.baseType;
			    for (int i = level; 
			         i >= linkInfo.level; i--)
			      prPush(i, 0);
			    prJS(linkInfo.labelNum);
			    for (int i = linkInfo.level; 
			         i <= level; i++)
			      prPop(i, 0);
                    }
                ;
N_PROCSTMT      : N_PROCIDENT
                    {
                    prRule("N_PROCSTMT", "N_PROCIDENT");
			    if ($1.type != PROCEDURE) {
			      yyerror("Procedure/variable mismatch");
			      return(0);
			    }
                    }
                ;
N_PROG          : N_PROGLBL T_IDENT T_SCOLON 
                    {
                    prRule("N_PROG",
                     "N_PROGLBL T_IDENT T_SCOLON N_BLOCK T_DOT");
			    string lexeme = string($2);
			    TYPE_INFO info = {PROGRAM, NOT_APPLICABLE,
				           NOT_APPLICABLE, NOT_APPLICABLE};
			    LINKAGE_INFO link = {NOT_APPLICABLE, level,
						 NOT_APPLICABLE,
                                  NOT_APPLICABLE};
			    prSymbolTableAddition(lexeme, info);
                    bool success = 
                          scopeStack.top().addEntry
                               (SYMBOL_TABLE_ENTRY
                                (lexeme,info,link));
			    }
			  N_BLOCK T_DOT
		         {
			    endScope();
                    }
                ;
N_PROGLBL       : T_PROG
                    {
                    prRule("N_PROGLBL", "T_PROG");
			    beginScope();
			    offset = 20;
                    }
                ;
N_READ          : T_READ T_LPAREN N_INPUTVAR N_INPUTLST T_RPAREN
                    {
                    prRule("N_READ",
                        "T_READ T_LPAREN N_INPUTVAR N_INPUTLST T_RPAREN");
										isAssign = false;
			   }
                ;
N_RELOP         : T_LT
                    {
                    prRule("N_RELOP", "T_LT");
			    $$ = T_LT;
                    }
                | T_GT
                    {
                    prRule("N_RELOP", "T_GT");
			    $$ = T_GT;
                    }
                | T_LE
                    {
                    prRule("N_RELOP", "T_LE");
			    $$ = T_LE;
                    }
                | T_GE
                    {
                    prRule("N_RELOP", "T_GE");
			    $$ = T_GE;
                    }
                | T_EQ
                    {
                    prRule("N_RELOP", "T_EQ");
			    $$ = T_EQ;
                    }
                | T_NE
                    {
                    prRule("N_RELOP", "T_NE");
			    $$ = T_NE;
                    }
                ;
N_SIGN          : /* epsilon */
                    {
                    prRule("N_SIGN", "epsilon");
			    $$ = NO_SIGN;
                    }
                | T_PLUS
                    {
                    prRule("N_SIGN", "T_PLUS");
			    $$ = POSITIVE;
                    }
                | T_MINUS
                    {
                    prRule("N_SIGN", "T_MINUS");
			    $$ = NEGATIVE;
                    }
                ;
N_SIMPLE        : T_INT
                    {
                    prRule("N_SIMPLE", "T_INT");
			    $$.type = INT; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                | T_CHAR
                    {
                    prRule("N_SIMPLE", "T_CHAR");
			    $$.type = CHAR; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                | T_BOOL
                    {
                    prRule("N_SIMPLE", "T_BOOL");
			    $$.type = BOOL; 
                    $$.startIndex = NOT_APPLICABLE;
                    $$.endIndex = NOT_APPLICABLE;
		         $$.baseType = NOT_APPLICABLE;
                    }
                ;
N_SIMPLEEXPR    : N_TERM N_ADDOPLST
                    {
                    prRule("N_SIMPLEEXPR", 
                           "N_TERM N_ADDOPLST");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_STMT          : N_ASSIGN
                    {
                    prRule("N_STMT", "N_ASSIGN");
                    }
                | N_PROCSTMT
                    {
                    prRule("N_STMT", "N_PROCSTMT");
                    }
                | N_READ
                    {
                    prRule("N_STMT", "N_READ");
                    }
                | N_WRITE
                    {
                    prRule("N_STMT", "N_WRITE");
                    }
                | N_CONDITION
                    {
                    prRule("N_STMT", "N_CONDITION");
                    }
                | N_WHILE
                    {
                    prRule("N_STMT", "N_WHILE");
                    }
                | N_COMPOUND
                    {
                    prRule("N_STMT", "N_COMPOUND");
                    }
                ;
N_STMTLST       : /* epsilon */
                    {
                    prRule("N_STMTLST", "epsilon");
                    }
                | T_SCOLON N_STMT N_STMTLST
                    {
                    prRule("N_STMTLST", 
                           "T_SCOLON N_STMT N_STMTLST");
                    }
                ;
N_STMTPART      : N_COMPOUND
                    {
                    prRule("N_STMTPART", "N_COMPOUND");
                    }
                ;
N_SUB_RANGE     : N_INTCONST T_DOTDOT N_INTCONST
                  {
									  $$.type = INT;
										$$.startIndex = $1;
										$$.endIndex = $3;
										$$.baseType = NOT_APPLICABLE;
									}
									|
									T_CHARCONST T_DOTDOT T_CHARCONST
									{
									  $$.type = CHAR;
										$$.startIndex = $1;
										$$.endIndex = $3;
										$$.baseType = NOT_APPLICABLE;
									}
								;
N_TERM          : N_FACTOR N_MULTOPLST
                    {
                    prRule("N_TERM", "N_FACTOR N_MULTOPLST");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                ;
N_TYPE          : N_SIMPLE
                    {
                    prRule("N_TYPE", "N_SIMPLE");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
                | N_ARRAY
                    {
                    prRule("N_TYPE", "N_ARRAY");
			    $$.type = $1.type; 
                    $$.startIndex = $1.startIndex;
                    $$.endIndex = $1.endIndex;
		         $$.baseType = $1.baseType;
                    }
								| N_SUB_RANGE
								{
								  
								}
                ;
N_VARDEC        : N_IDENT N_IDENTLST T_COLON N_TYPE 
                    {
                    prRule("N_VARDEC", 
                           "N_IDENT N_IDENTLST T_COLON N_TYPE");
			    string varName = string($1);
			    variableNames.push_front(varName);
		
			    LINKAGE_INFO link;
			    link.level = level;
			    link.labelNum = NOT_APPLICABLE;
    			    link.frameSize = NOT_APPLICABLE;
			    for (std::list<string>::iterator
                          it=variableNames.begin();
			          it != variableNames.end(); it++) {
				 string varName = string(*it);
			       // Store array offset differently
			       if ($4.type == ARRAY)
				   link.offset = offset - $4.startIndex;
			       else link.offset = offset;
			       prSymbolTableAddition(varName, $4);
                       bool success = scopeStack.top().addEntry
                               (SYMBOL_TABLE_ENTRY(varName, 
									  $4, link));
//printf("Added %s with offset %d\n", $1, link.offset);
                	       if (! success) {
                         yyerror("Multiply defined identifier");
                         return(0);
               	       }
                       if ($4.type == ARRAY) {
                         if ($4.startIndex > $4.endIndex) {
                           yyerror("Start index must be less than or equal to end index of array");
                           return(0);
                         }
                       }
				 // set next var's offset
				 if ($4.type != ARRAY)
				   offset = offset + 1;
				 else offset = offset + 
				          ($4.endIndex-$4.startIndex+1);
				}
                     variableNames.clear();
                    }
                ;
N_VARDECLST     : /* epsilon */
                    {
                    prRule("N_VARDECLST", "epsilon");
                    }
                | N_VARDEC T_SCOLON N_VARDECLST
                    {
                    prRule("N_VARDECLST", 
                           "N_VARDEC T_SCOLON N_VARDECLST");
                   }
                ;
N_VARDECPART    : /* epsilon */
                    {
                    prRule("N_VARDECPART", "epsilon");
                    }
                | T_VAR N_VARDEC T_SCOLON N_VARDECLST
                    {
                    prRule("N_VARDECPART",
                        "T_VAR N_VARDEC T_SCOLON N_VARDECLST");
                    }
                ;
N_VARIABLE      : N_ENTIREVAR
                    {
										prRule("N_VARIABLE", "N_ENTIREVAR");
			    $$.type = $1.type;
			    $$.startIndex = $1.startIndex;
			    $$.endIndex = $1.endIndex;
			    $$.baseType = $1.baseType;
                    }
                | N_IDXVAR
                    {
                    prRule("N_VARIABLE", "N_IDXVAR");
			    $$.type = $1.type;
			    $$.startIndex = $1.startIndex;
			    $$.endIndex = $1.endIndex;
			    $$.baseType = $1.baseType;
                    }
                ;
N_VARIDENT      : T_IDENT
                    {
                    prRule("N_VARIDENT", "T_IDENT");
			    string ident = string($1);
                	    TYPE_INFO typeInfo;
			    LINKAGE_INFO linkInfo;
                	    if (!findEntryInAnyScope(ident,
                                           typeInfo,linkInfo)){
                	      yyerror("Undefined identifier");
                	      return(0);
               	    }
			    if (typeInfo.type == PROCEDURE) {
			      yyerror("Procedure/variable mismatch");
                      return(0);
			    }
			    $$.type = typeInfo.type;
			    $$.startIndex = typeInfo.startIndex;
			    $$.endIndex = typeInfo.endIndex;
			    $$.baseType = typeInfo.baseType;
			    prLA(linkInfo.offset, linkInfo.level);
					readOffset = linkInfo.offset;
					readLevel = linkInfo.level;
					if(isAssign == false)
					{
					  isAssign = true;
						assignOffset = readOffset;
						assignLevel = readLevel;
					}
			    }
                ;
N_WHILE         : T_WHILE
		  	   {
		         prRule("N_WHILE", 
                           "T_WHILE N_EXPR T_DO N_STMT");
			   $1 = labelNum;
			   prLabel(labelNum);
			   labelNum = labelNum + 2;  
			   }
			  N_EXPR  
                    {
			    if ($3.type != BOOL) {
			      yyerror("Expression must be of type boolean");
			      return(0);
			    }
			    prJF($1+1);
                    }
			  T_DO N_STMT
			   {
			    prJP($1);
			    prLabel($1+1);
			   }
                ;
N_WRITE         : T_WRITE T_LPAREN N_OUTPUT N_OUTPUTLST T_RPAREN
                    {
                    prRule("N_WRITE",
                      "T_WRITE T_LPAREN N_OUTPUT N_OUTPUTLST T_RPAREN");
                    }
                ;
%%

#include    "lex.yy.c"
extern FILE *yyin;

void prLabel(const int lnum) 
{
  printf("L.%d:\n", lnum);
  return;
}

void prAsp(const int offset) 
{
  if (offset)
    printf("  asp %d\n", offset);
  return;
}

void prBss(const int s) 
{
  printf("  bss %d\n", s);
  return;
}

void prComment(const char *s, const int offset) 
{
  if (!offset) printf("\n");
  printf("%*s# %s\n", offset, "", s);
  return;
}

void prDeref() 
{
  printf("  deref\n");
  return;
}

void prEnd() 
{
  printf("  end\n");
  return;
}

void prHalt() 
{
  printf("  halt\n");
  return;
}

void prInit(const int g, const int d, const int s, 
            const int c, const int e) 
{
  printf("  init L.%d, %d, L.%d, L.%d, L.%d\n",
	    g, d, s, c, e);
  return;
}

void prLA(const int offset, const int level) 
{
  printf("  la %d, %d\n", offset, level);
  return;
}

void prLC(const int val) 
{
  printf("  lc %d\n", val);
  return;
}

void prRead(const int varType)
{
  if (varType == INT)
    printf("  iread\n");
  else printf("  cread\n");
  printf("  st\n");
  return;
}

void prSave(const int level) 
{
  printf("  save %d, 0\n", level);
  return;
}

void prST() 
{
  printf("  st\n");
  return;
}

void prWrite(const int varType) 
{
  if (varType == INT)
    printf("  iwrite\n");
  else printf("  cwrite\n");
  return;
}

void prJF(const int lab) 
{
  printf("  jf L.%d\n", lab);
  return;
}

void prJP(const int lab) 
{
  printf("  jp L.%d\n", lab);
  return;
}

void prJI()
{
  printf("  ji\n");
  return;
}

void prJS(const int lab) 
{
  printf("  js L.%d\n", lab);
  return;
}

void prOp(const int op) 
{
  char s[10];
  switch (op)
  {
	case T_NEG:	strcpy(s,"neg");	 break;
	case T_PLUS:	strcpy(s, "add"); break;
	case T_MINUS:	strcpy(s, "sub"); break;
	case T_MULT:	strcpy(s, "mult"); break;
	case T_DIV:	strcpy(s, "div"); break;
	case T_NOT:	strcpy(s, "not"); break;
	case T_AND:	strcpy(s, "and"); break;
	case T_OR:		strcpy(s, "or");	 break;
	case T_EQ:		strcpy(s, ".eq."); break;
	case T_NE:		strcpy(s, ".ne."); break;
	case T_GT:		strcpy(s, ".gt."); break;
	case T_LT:		strcpy(s, ".lt."); break;
	case T_GE:		strcpy(s, ".ge."); break;
	case T_LE:		strcpy(s, ".le."); break;
	default:		strcpy(s, "??"); break;
  }
  printf("  %s\n", s);
  return;
}

void prPush(const int off, const int lev) 
{
  printf("  push %d, %d\n", off, lev);
  return;
}

void prPop(const int off, const int lev) 
{
  printf("  pop %d, %d\n", off, lev);
  return;
}

void prRule(const char *lhs, const char *rhs) 
{
  if (OUTPUT_PRODUCTIONS)
    printf("%s -> %s\n", lhs, rhs);
  return;
}

int yyerror(const char *s) 
{
  printf("Line %d: %s\n", lineNum, s);
  cleanUp();
  exit(1);
}

int ckInt() 
{
  char *ptr;
  int	rc = 0;
  ptr = yytext;

  /* ignore sign and leading zeroes */
  if (*ptr == '-' || *ptr == '+')
    ++ptr;
  while (*ptr == '0')
    ++ptr;

  switch (*ptr) 
  {
  case '1':	/* ALL are valid */
			break;

  case '2':	/* it depends */
			if (strcmp(MAX_INT, ptr) < 0)
				rc = 1;
			break;

  default:	     /* ALL are invalid */
			rc = 1;
			break;
  }
  return rc;
}

void printTokenInfo(const char* tokenType, 
                    const char* lexeme) 
{
  if (OUTPUT_TOKENS)
    printf("TOKEN: %-15s  LEXEME: %s\n", tokenType, lexeme);
}

void ignoreComment() 
{
  char c, pc = 0;

  /* read and ignore input until you get an ending token */
  while (((c = yyinput()) != ')' || pc != '*') && c != 0) 
  {
    pc = c;
    if (c == '\n') lineNum++;
  }

  return;
}

void beginScope() 
{
  scopeStack.push(SYMBOL_TABLE());
  if (OUTPUT_ST_MGT) 
    printf("\n___Entering new scope...\n\n");
}

void endScope() 
{
  scopeStack.pop();
  if (OUTPUT_ST_MGT) 
    printf("\n___Exiting scope...\n\n");
}

void prSymbolTableAddition(const string identName, 
                           const TYPE_INFO typeInfo) 
{
  if (OUTPUT_ST_MGT) 
  {
   char *cstr = new char[identName.length() + 1];
   strcpy(cstr, identName.c_str());
   printf("___Adding %s to symbol table with type ", cstr);
   delete [] cstr;
   switch (typeInfo.type) 
   {
	case PROGRAM	: printf("PROGRAM\n"); break;
	case PROCEDURE	: printf("PROCEDURE\n"); break;
	case INT		: printf("INTEGER\n"); break;
	case CHAR		: printf("CHAR\n"); break;
	case BOOL		: printf("BOOLEAN\n"); break;
	case ARRAY		: printf("ARRAY ");
				  printf("%d .. %d OF ",
				         typeInfo.startIndex, 
				         typeInfo.endIndex);
				  switch (typeInfo.baseType) 
                        {
				    case INT : printf("INTEGER\n"); break;
				    case CHAR: printf("CHAR\n"); break;
				    case BOOL: printf("BOOLEAN\n"); break;
				    default : printf("UNKNOWN\n"); break;
				  }
				  break;
	default 		: printf("UNKNOWN\n"); break;
   }
  }
}

bool findEntryInAnyScope(const string theName,
                         TYPE_INFO &typeInfo,
				   LINKAGE_INFO &linkInfo) 
{
  bool success;
  if (scopeStack.empty()) return(false);
  success = scopeStack.top().findEntry(theName, 
                                       typeInfo, linkInfo);
  if (success)
    return(true);
  else { // check in "next higher" scope
	   SYMBOL_TABLE symbolTable = scopeStack.top( );
	   scopeStack.pop( );
	   success = findEntryInAnyScope(theName, 
                                      typeInfo, linkInfo);
	   scopeStack.push(symbolTable); // restore the stack
	   return(success);
  }
}

void cleanUp() 
{
  if (scopeStack.empty()) 
    return;
  else {
        scopeStack.pop();
        cleanUp();
  }
}

int main(int argc, char** argv) 
{
  if (argc < 2) {
    printf("You must specify a file in the command line!\n");
    exit(1);
  }
  yyin = fopen(argv[1], "r");
  do {
	yyparse();
  } while (!feof(yyin));
  return 0;
}


